> {-# LANGUAGE OverloadedStrings #-}

Send a SAMP message (with potential arguments) to all interested clients.

Usage:

   ./sender [sync|async|notify] mtype [param1=value1] [param2=value2] ... [paramN=valueN]

TODO:

  - target name(s) as well as ids

  - sendername/metadata

  - improved error handling

  - better checks of responses

  - better command-line handling

> module Main where
>
> import System.Environment (getArgs, getProgName)
> import System.Exit (exitSuccess, exitFailure)
> import System.IO
> import System.IO.Error
> import System.Random
>
> import qualified Control.Exception as CE
> import Control.Concurrent
> import Control.Concurrent.ParallelIO.Global
>
> import Control.Monad (msum, forM_, unless, when)
> import Control.Monad.Trans (liftIO)
> import Data.Maybe (fromJust)
>
> import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
>
> import qualified Data.ByteString.Char8 as B
> import qualified Data.ByteString.Lazy.Char8 as L
>
> -- import System.Log.Logger
>
> import Network.SAMP.Standard
>
> import Network.Socket (Socket, PortNumber, socketPort)
> import Happstack.Server.SimpleHTTP
>

> timeout :: Int
> timeout = 10

> sleep :: Int -> IO ()
> sleep = threadDelay . (1000000 *)

> -- unsafe conversion routine
> tRS :: String -> RString
> tRS = fromJust . toRString
>
> usage :: IO ()
> usage = getProgName >>= \n ->
>     hPutStrLn stderr $ "Usage: " ++ n ++ " [sync|async|notify] mtype [param1=value1] .. [paranN=valueN]"
>
> data ConnMode = Sync | ASync | Notify deriving (Eq, Show)

> getConnection :: [String] -> (ConnMode, [String])
> getConnection ("sync":xs) = (Sync, xs)
> getConnection ("async":xs) = (ASync, xs)
> getConnection ("notify":xs) = (Notify, xs)
> getConnection xs = (Sync, xs)

> getMType :: [String] -> Either String (MType, [String])
> getMType [] = Left "Missing required mtype argument"
> getMType (name:xs) = 
>     handleError Left $ fmap (flip (,) xs) (toMTypeE name)

> getKV :: String -> Either String SAMPKeyValue
> getKV arg = do
>     let (l,r) = break (=='=') arg
>     lval <- if null l
>               then Left $ "Expected key=value but sent '" ++ arg ++ "'"
>               else Right l
>     rval <- if null r || r == "="
>               then Left $ "Expected key=value but sent '" ++ arg ++ "'"
>               else Right $ tail r
>     handleError Left $ stringToKeyValE lval rval

> processArgs :: [String] -> Either String (ConnMode, MType, [SAMPKeyValue])
> processArgs args = do
>     let (conmode, args1) = getConnection args
>     (mtype, args2) <- getMType args1
>     kvals <- mapM getKV args2
>     return (conmode, mtype, kvals)

> main :: IO ()
> main = do
>     args <- getArgs
>     case processArgs args of
>         Left emsg -> hPutStrLn stderr ("ERROR: " ++ emsg ++ "\n") >> usage >> exitFailure
>         Right x -> 
>             do
>               conn <- doE createClient
>
>               -- The assumption is that we only need to unregister if we get
>               -- a user interrupt but not for other errors. This is actually not
>               -- correct since we use runE/fail within processMessage for situations
>               -- where we need to clean up. So also look for user exceptions.
>
>               let asyncHdlr :: CE.AsyncException -> IO ()
>                   asyncHdlr e = when (e == CE.UserInterrupt) (doE (unregisterE conn)) >>
>                                   hPutStrLn stderr ("ERROR: " ++ show e) >> exitFailure
>     
>                   ioHdlr :: CE.IOException -> IO ()
>                   ioHdlr e | isUserError e = doE (unregisterE conn) >>
>                                                hPutStrLn stderr ("ERROR: " ++ ioeGetErrorString e) >> exitFailure
>                            | otherwise     = hPutStrLn stderr ("ERROR: " ++ show e) >> exitFailure
>
>                   otherHdlr :: CE.SomeException -> IO ()
>                   otherHdlr e = putStrLn ("ERROR: " ++ show e) >> exitFailure
>
>               processMessage conn x `CE.catches`
>                   [CE.Handler ioHdlr, CE.Handler asyncHdlr, CE.Handler otherHdlr]
>               doE (unregisterE conn)
>               exitSuccess

Run a set of SAMP commands and exit on error.

> doE :: Err IO a -> IO a
> doE = handleError
>         (\emsg -> hPutStrLn stderr ("ERROR: " ++ emsg) >> exitFailure)

> authorMetadata :: [SAMPKeyValue]
> authorMetadata = 
>     [("author.name", "Doug Burke"),
>      ("author.affiliation", "Smithsonian Astrophysical Observatory"),
>      ("author.mail", "dburke@cfa.harvard.edu")]

Set up a simple client (i.e. with limited metadata)

> createClient :: Err IO SAMPConnection
> createClient =
>      getHubInfoE >>=
>      registerClientE >>= \conn ->
>      toMetadataE "hsamp-sender" (Just "Send a message to interested clients.")
>          Nothing Nothing Nothing >>= \md ->
>      declareMetadataE conn (md ++ authorMetadata) >>
>      return conn

There is a chance that targets may be added or removed in between the
getSubscribedClientsE call and the later processing, but we except this
for now (for the synchronous case we need to use the getSubscribedClientsE
route, but not for the other two methods which could skip this
step).

> processMessage :: SAMPConnection -> (ConnMode, MType, [SAMPKeyValue]) -> IO ()
> processMessage conn (cmode, mtype, params) = do
>     targets <- runE (getSubscribedClientsE conn mtype)
>     if null targets
>       then putStrLn ("No clients are subscribed to the message " ++ show mtype)
>       else do
>           msg <- runE (toSAMPMessage mtype params)
>           barrier <- newBarrier -- not needed for notification case but do anyway
>           case cmode of
>             Sync   -> parallel_ (map (sendSync barrier conn msg . fst) targets) >> stopGlobalPool
>             ASync  -> do
>                         chan <- newChannel
>                         tvar <- emptyTimeVar
>                         _ <- makeServer barrier tvar chan conn
>                         clients <- sendASync tvar conn msg
>
>                         bv <- newEmptyMVar
>                         cv <- newMVar clients
>                         _ <- forkIO (waitForCalls chan cv >> putMVar bv True)
>                         _ <- forkIO (sleep timeout >> putMVar bv False)
>                         flag <- takeMVar bv
>                         unless flag $ do
>                           rclients <- takeMVar cv 
>                           -- even if flag is True there is the possibility that all
>                           -- the clients responded (as we haven't explicitly halted the
>                           -- waitForCalls thread here) 
>                           --
>                           -- since we got no response we won't bother asking for their samp.name
>                           -- settings either
>                           unless (null rclients) $ case rclients of
>                               [cl] -> fail $ "The following client failed to respond: " ++ fromRString cl
>                               _ -> fail $ "The following clients failed to respond: " ++
>                                       unwords (map fromRString rclients)
>                         
>             Notify -> do
>                         putStrLn "Notifications sent to:" 
>                         tgts <- runE (notifyAllE conn msg >>= mapM (\n -> fmap ((,) n) (getClientNameE conn n)))
>                         forM_ tgts $ \t -> putStrLn ("    " ++ showTarget t)

> type Barrier = MVar ()
> type TimeVar = MVar UTCTime
> type Channel = Chan RString

> newBarrier :: IO Barrier
> newBarrier = newMVar ()

> emptyTimeVar :: IO TimeVar
> emptyTimeVar = newEmptyMVar

> newChannel :: IO Channel
> newChannel = newChan

> printResponse :: Barrier -> NominalDiffTime -> (RString, Maybe RString) -> SAMPResponse -> IO ()
> printResponse barrier delta target rsp
>     | isSAMPErrorOnly rsp = syncAction_ barrier $ printError delta target rsp
>     | isSAMPWarning rsp   = syncAction_ barrier $ printWarning delta target rsp
>     | otherwise           = syncAction_ barrier $ printSuccess delta target rsp

TODO: convert to syncPrint from Snooper.lhs 

> syncAction_ :: Barrier -> IO () -> IO ()
> syncAction_ barrier = withMVar barrier . const 

> showKV :: SAMPKeyValue -> IO ()
> showKV (k,v) = putStrLn $ "    " ++ fromRString k ++ " -> " ++ show v

> showTarget :: (RString, Maybe RString) -> String
> showTarget (tid, Nothing) = fromRString tid
> showTarget (tid, Just tname) = fromRString tid ++ " (" ++ fromRString tname ++ ")"

> printHeader :: String -> NominalDiffTime -> (RString, Maybe RString) -> IO ()
> printHeader lbl delta target = 
>     putStrLn $ lbl ++ " response from " ++ showTarget target ++ " in " ++ show delta ++ " seconds"

> printSuccess :: NominalDiffTime -> (RString, Maybe RString) -> SAMPResponse -> IO ()
> printSuccess delta target rsp = do
>     printHeader "Successful" delta target
>     let Just svals = getSAMPResponseResult rsp
>     forM_ svals showKV

> printError :: NominalDiffTime -> (RString, Maybe RString) -> SAMPResponse -> IO ()
> printError delta target rsp = do
>     printHeader "Error" delta target
>     let Just (emsg, evals) = getSAMPResponseError rsp
>     putStrLn $ "    " ++ fromRString emsg
>     forM_ evals showKV

> printWarning :: NominalDiffTime -> (RString, Maybe RString) -> SAMPResponse -> IO ()
> printWarning delta target rsp = do
>     printHeader "Warning" delta target
>     let Just svals = getSAMPResponseResult rsp
>         Just (emsg, evals) = getSAMPResponseError rsp
>     putStrLn $ "    " ++ fromRString emsg
>     unless (null svals) $ do
>         putStrLn "  Response"
>         forM_ svals showKV
>     unless (null evals) $ do
>         putStrLn "  Error"
>         forM_ evals showKV

> sendSync :: Barrier -> SAMPConnection -> SAMPMessage -> RString -> IO ()
> sendSync barrier conn msg tid = do
>     sTime <- getCurrentTime
>     rsp <- runE (callAndWaitE conn tid msg (Just timeout))
>     eTime <- getCurrentTime
>     tname <- handleError (return . const Nothing) $ getClientNameE conn tid
>     printResponse barrier (diffUTCTime eTime sTime) (tid,tname) rsp

> getMsgId :: IO RString
> getMsgId = getStdRandom $ randomRString 10

> kvToRSE :: (Monad m) => SAMPKeyValue -> Err m (RString, RString)
> kvToRSE (k,SAMPString v) = return (k, v)
> kvToRSE (k,v) = fail $ "Key " ++ fromRString k ++ " should be a SAMP string but found " ++ show v

TODO: need to handle errors more sensibly than runE here!

> sendASync :: TimeVar -> SAMPConnection -> SAMPMessage -> IO [RString]
> sendASync mt conn msg = do
>     sTime <- getCurrentTime
>     putMVar mt sTime
>     msgId <- getMsgId
>     fmap (map fst) $ runE (callAllE conn msgId msg >>= mapM kvToRSE)

> waitForCalls :: Channel -> MVar [RString] -> IO ()
> waitForCalls chan cv = do
>    receiverid <- readChan chan
>    modifyMVar_ cv $ \clients -> 
>      if receiverid `elem` clients
>        then return $ filter (/= receiverid) clients
>        else do
>           putStrLn $ "Ignoring unexpected response from " ++ fromRString receiverid
>           return clients
>    ncl <- readMVar cv
>    unless (null ncl) $ waitForCalls chan cv

Basic configuration for setting up the server.

> eConf :: Conf
> eConf = nullConf { port = 0 }

The location of the server we create to handle incoming messages from
the SAMP hub. Note that this includes the XML-RPC route since in this
case we do not support any other access.

> hostName :: PortNumber -> String
> hostName portNum = "http://127.0.0.1:" ++ show portNum ++ "/xmlrpc"

TODO: should we respond to the hub shutting down? Almost certainly
(especially for the synchronous case where we currently aren't planning
on setting up the server).

> makeServer :: Barrier -> TimeVar -> Channel -> SAMPConnection -> IO ThreadId
> makeServer barrier mt chan conn = do
>     -- what is the address of the server?
>     sock <- bindPort eConf
>     pNum <- socketPort sock
>     url <- runE $ toRStringE (hostName pNum)
>
>     -- register the messages that we are interested in
>     runE $ setXmlrpcCallbackE conn url >>
>            declareSubscriptionsSimpleE conn []
>
>     -- now run the server
>     forkIO (runServer barrier mt chan sock pNum conn)

Creates the server, listening to the assigned socket/port.

> runServer :: Barrier -> TimeVar -> Channel -> Socket -> PortNumber -> SAMPConnection -> IO ()
> runServer barrier mt chan sock portNum conn = do
>      tid <- myThreadId
>      simpleHTTPWithSocket sock (nullConf { port = fromEnum portNum }) 
>          (handlers barrier mt chan conn tid)

> handlers :: Barrier -> TimeVar -> Channel -> SAMPConnection -> ThreadId -> ServerPart Response
> handlers barrier mt chan conn tid =
>     msum [handleXmlRpc barrier mt chan conn tid,
>           anyRequest (notFound (toResponse ("unknown request" :: String)))]

XML-RPC is done via a HTML POST of an XML document.
At present I enforce the content-type restriction but
this may be a bit OTT.

> xmlct :: B.ByteString
> xmlct = B.pack "text/xml"

> handleXmlRpc :: Barrier -> TimeVar -> Channel -> SAMPConnection -> ThreadId -> ServerPart Response
> handleXmlRpc barrier mt chan conn tid = dir "xmlrpc" $ do
>     methodM POST
>     cType <- getHeaderM "content-type"
>     unless (cType == Just xmlct) $ escape (badRequest (toResponse ("Invalid content type: " ++ show cType)))
>     r <- askRq
>     getResponse barrier mt chan conn tid (rqBody r)

> getResponse :: Barrier -> TimeVar -> Channel -> SAMPConnection -> ThreadId -> RqBody -> ServerPart Response
> getResponse barrier mt chan conn tid (Body bdy) = do
>     let call = L.unpack bdy
>     liftIO $ simpleClientServer conn (notifications tid) calls (rfunc conn barrier mt chan) call
>     ok (toResponse ("" :: String))

TODO: support hub shutdown

> allMT :: MType
> allMT = "*"

> notifications :: ThreadId -> [SAMPNotificationFunc]
> notifications _ =
>      [(allMT, handleOther)]

> calls :: [SAMPCallFunc]
> calls = []

> handleOther :: MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleOther mtype _ name keys = do
>     putStrLn $ "Notification of " ++ show mtype ++ " from " ++ fromRString name
>     forM_ keys showKV
>     putStrLn ""

TODO: check that msgid is correct, which means it has to be sent in!

> rfunc :: SAMPConnection -> Barrier -> TimeVar -> Channel -> SAMPResponseFunc
> rfunc conn barrier mt chan _ tid _ rsp = do
>    eTime <- getCurrentTime
>    sTime <- readMVar mt
>    tname <- handleError (return . const Nothing) $ getClientNameE conn tid
>    printResponse barrier (diffUTCTime eTime sTime) (tid,tname) rsp
>    writeChan chan tid


