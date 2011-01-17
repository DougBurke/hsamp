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
> import Control.Monad (forM_, unless, when)
>
> import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
>
> -- import System.Log.Logger
>
> import Network.SAMP.Standard
>
> import qualified Network as N
> import Network.Socket (Socket)
> import Server

> timeout :: Int
> timeout = 10

> sleep :: Int -> IO ()
> sleep = threadDelay . (1000000 *)                                                            

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
>             withSAMP $ do
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
>                   otherHdlr e = hPutStrLn stderr ("ERROR: " ++ show e) >> exitFailure
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
>       then putStrLn $ "No clients are subscribed to the message " ++ show mtype
>       else do
>           msg <- runE (toSAMPMessage mtype params)
>           barrier <- newBarrier -- not needed for notification case but do anyway
>           case cmode of
>             Sync   -> parallel_ (map (sendSync barrier conn msg . fst) targets) >> stopGlobalPool
>             ASync  -> do
>                         chan <- newChannel
>                         tvar <- emptyTimeVar
>                         
>                         sock <- getSocket
>                         _ <- makeServer barrier tvar chan conn sock
>                         clients <- sendASync tvar conn msg
>
>                         bv <- newEmptyMVar
>                         cv <- newMVar clients
>                         _ <- forkIO (waitForCalls barrier chan cv >> putMVar bv True)
>                         _ <- forkIO (sleep timeout >> putMVar bv False)
>                         flag <- takeMVar bv
>
>                         -- do we need to do this? should this be done within a resource
>                         -- "handler" (to make sure the socket is closed on error)?
>                         N.sClose sock
>                         
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

NOTE: this uses putStr rather than putStrLn, so assumes the string
ends in "\n"

> syncPrint :: Barrier -> String -> IO ()
> syncPrint barrier msg = forkIO (withMVar barrier $ \_ -> putStr msg) >> return ()

> printResponse :: Barrier -> NominalDiffTime -> (RString, Maybe RString) -> SAMPResponse -> IO ()
> printResponse barrier delta target rsp
>     | isSAMPErrorOnly rsp = syncPrint barrier $ showError delta target rsp
>     | isSAMPWarning rsp   = syncPrint barrier $ showWarning delta target rsp
>     | otherwise           = syncPrint barrier $ showSuccess delta target rsp

> showKV :: SAMPKeyValue -> String
> showKV (k,v) = "    " ++ fromRString k ++ " -> " ++ show v

> showTarget :: (RString, Maybe RString) -> String
> showTarget (tid, Nothing) = fromRString tid
> showTarget (tid, Just tname) = fromRString tid ++ " (" ++ fromRString tname ++ ")"

> showHeader :: String -> NominalDiffTime -> (RString, Maybe RString) -> String
> showHeader lbl delta target = 
>     lbl ++ " response from " ++ showTarget target ++ " in " ++ show delta ++ " seconds"

> showSuccess :: NominalDiffTime -> (RString, Maybe RString) -> SAMPResponse -> String
> showSuccess delta target rsp = 
>     let Just svals = getSAMPResponseResult rsp  
>     in unlines $ showHeader "Successful" delta target : map showKV svals

> showError :: NominalDiffTime -> (RString, Maybe RString) -> SAMPResponse -> String
> showError delta target rsp = 
>     let Just (emsg, evals) = getSAMPResponseError rsp
>     in unlines $ [showHeader "Error" delta target, "    " ++ fromRString emsg]
>                  ++ map showKV evals

> showWarning :: NominalDiffTime -> (RString, Maybe RString) -> SAMPResponse -> String
> showWarning delta target rsp = 
>     let Just svals = getSAMPResponseResult rsp
>         Just (emsg, evals) = getSAMPResponseError rsp
>     in unlines $ [showHeader "Warning" delta target, "    " ++ fromRString emsg]
>                  ++ if null svals then [] else "  Response" : map showKV svals
>                  ++ if null evals then [] else "  Error" : map showKV evals

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

> waitForCalls :: Barrier -> Channel -> MVar [RString] -> IO ()
> waitForCalls barrier chan cv = do
>    receiverid <- readChan chan
>    modifyMVar_ cv $ \clients -> 
>      if receiverid `elem` clients
>        then return $ filter (/= receiverid) clients
>        else do
>           syncPrint barrier $ "Ignoring unexpected response from " ++ fromRString receiverid ++ "\n"
>           return clients
>    ncl <- readMVar cv
>    unless (null ncl) $ waitForCalls barrier chan cv

TODO: should we respond to the hub shutting down? Almost certainly
(especially for the synchronous case where we currently aren't planning
on setting up the server).

> makeServer :: Barrier -> TimeVar -> Channel -> SAMPConnection -> Socket -> IO ThreadId
> makeServer barrier mt chan conn sock = do
>     tid <- myThreadId
>     url <- getAddress sock
>     urlR <- runE (toRStringE url)
>
>     -- register the messages that we are interested in
>     runE $ setXmlrpcCallbackE conn urlR >>
>            declareSubscriptionsSimpleE conn []
>
>     -- now run the server
>     forkIO $ runServer sock $ processCall barrier mt chan conn tid

> processCall :: Barrier -> TimeVar -> Channel -> SAMPConnection -> ThreadId -> String -> IO ()
> processCall barrier mt chan conn tid =
>     simpleClientServer conn (notifications barrier tid) calls (rfunc conn barrier mt chan) 

TODO: support hub shutdown

> allMT :: MType
> allMT = "*"

> notifications :: Barrier -> ThreadId -> [SAMPNotificationFunc]
> notifications barrier _ =
>      [(allMT, handleOther barrier)]

> calls :: [SAMPCallFunc]
> calls = []

> handleOther :: Barrier -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleOther barrier mtype _ name keys = syncPrint barrier $ unlines $
>     ("Notification of " ++ show mtype ++ " from " ++ fromRString name) :
>     map showKV keys ++ [""]

TODO: check that msgid is correct, which means it has to be sent in!

> rfunc :: SAMPConnection -> Barrier -> TimeVar -> Channel -> SAMPResponseFunc
> rfunc conn barrier mt chan _ tid _ rsp = do
>    eTime <- getCurrentTime
>    sTime <- readMVar mt
>    tname <- handleError (return . const Nothing) $ getClientNameE conn tid
>    printResponse barrier (diffUTCTime eTime sTime) (tid,tname) rsp
>    writeChan chan tid


