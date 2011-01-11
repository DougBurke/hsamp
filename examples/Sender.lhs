
ghc -hide-package monads-fd --make -o sender -Wall Sender.lhs -i..

Send a SAMP message (with potential arguments) to all interested clients.

Usage:

   ./sender [sync|async|notify] mtype [param1=value1] [param2=value2] ... [paramN=valueN]

TODO:

  - target name(s)

  - sendername/metadata

  - improved error handling

  - better checks of responses

  - time out for missing responses from async mode

> module Main where
>
> import System.Environment (getArgs, getProgName)
> import System.Exit (exitSuccess, exitFailure)
> import System.IO
> import System.IO.Error
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
> import System.Log.Logger
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
>     case toMType name of
>         Just mtype -> Right (mtype, xs)
>         _ -> Left $ "Invalid mtype: " ++ name
                     
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
>     kvals <- sequence (map getKV args2)
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
>               processMessage conn x `CE.catches` [CE.Handler ioHdlr, CE.Handler asyncHdlr, CE.Handler otherHdlr]
>               doE (unregisterE conn)
>               exitSuccess

Run a set of SAMP commands and exit on error.

> doE :: Err IO a -> IO a
> doE = handleError
>         (\emsg -> hPutStrLn stderr ("ERROR: " ++ emsg) >> exitFailure)

> authorMetadata :: Err IO SAMPKeyValue
> authorMetadata = stringToKeyValE "author.mail" "dburke@cfa.harvard.edu"

Set up a simple client (i.e. with limited metadata)

> createClient :: Err IO SAMPConnection
> createClient =
>      authorMetadata >>= \amd ->
>      getHubInfoE >>=
>      registerClientE >>= \conn ->
>      toMetadataE "sender" (Just "Send a message to interested clients.")
>          Nothing Nothing Nothing >>= \md ->
>      declareMetadataE conn (amd:md)>>
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
>           mv <- newMVar () -- not needed for notification but do anyway
>           case cmode of
>             Sync   -> parallel_ (map (sendSync mv conn msg . fst) targets) >> stopGlobalPool
>             ASync  -> do
>                         chan <- newChan
>                         mt <- newEmptyMVar
>                         _ <- makeServer mv mt chan conn
>                         clients <- sendASync mt conn msg
>                         bv <- newEmptyMVar
>                         _ <- forkIO (sleep timeout >> putMVar bv False)
>                         _ <- forkIO (waitForCalls chan clients >> putMVar bv True)
>                         -- TODO: more work to report which clients did not respons
>                         flag <- takeMVar bv
>                         unless flag $ fail "At least one client failed to respond!"
>                         
>             Notify -> putStrLn "Notifications sent to:" >> runE (notifyAllE conn msg) >>=
>                       mapM_ (\n -> putStrLn ("    " ++ show n))

> type Barrier = MVar ()

> printResponse :: Barrier -> NominalDiffTime -> RString -> SAMPResponse -> IO ()
> printResponse mv delta target rsp = 
>     if isSAMPErrorOnly rsp
>         then syncAction mv $ printError delta target rsp
>         else if isSAMPWarning rsp
>           then syncAction mv $ printWarning delta target rsp
>           else syncAction mv $ printSuccess delta target rsp

> syncAction :: Barrier -> IO () -> IO ()
> syncAction mv act = withMVar mv $ const act

> printSuccess :: NominalDiffTime -> RString -> SAMPResponse -> IO ()
> printSuccess delta target rsp = do
>     putStrLn $ "Successful response from " ++ show target ++ " (" ++ show delta ++ " seconds)"
>     let Just svals = getSAMPResponseResult rsp
>     forM_ svals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v

> printError :: NominalDiffTime -> RString -> SAMPResponse -> IO ()
> printError delta target rsp = do
>     putStrLn $ "Error response from " ++ show target ++ " (" ++ show delta ++ " seconds)"
>     let Just (emsg, evals) = getSAMPResponseError rsp
>     putStrLn $ "    " ++ show emsg
>     forM_ evals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v

> printWarning :: NominalDiffTime -> RString -> SAMPResponse -> IO ()
> printWarning delta target rsp = do
>     putStrLn $ "Warning response from " ++ show target ++ " (" ++ show delta ++ " seconds)"
>     let Just svals = getSAMPResponseResult rsp
>         Just (emsg, evals) = getSAMPResponseError rsp
>     putStrLn $ "    " ++ show emsg
>     unless (null svals) $ do
>         putStrLn $ "  Response"
>         forM_ svals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v
>     unless (null evals) $ do
>         putStrLn $ "  Error"
>         forM_ evals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v

> sendSync :: Barrier -> SAMPConnection -> SAMPMessage -> RString -> IO ()
> sendSync mv conn msg target = do
>     sTime <- getCurrentTime
>     rsp <- runE (callAndWaitE conn target msg (Just timeout))
>     eTime <- getCurrentTime
>     printResponse mv (diffUTCTime eTime sTime) target rsp

Hardly unique!

> msgId :: RString
> msgId = fromJust (toRString "unique-identifier-string-honest!")

> kvToRSE :: (Monad m) => SAMPKeyValue -> Err m (RString, RString)
> kvToRSE (k,SAMPString v) = return (k, v)
> kvToRSE (k,v) = fail $ "Key " ++ show k ++ " should be a SAMP string but found " ++ show v

TODO: need to handle errors more sensibly than runE here!

> sendASync :: MVar UTCTime -> SAMPConnection -> SAMPMessage -> IO [RString]
> sendASync mt conn msg = do
>     sTime <- getCurrentTime
>     putMVar mt sTime
>     runE (callAllE conn msgId msg >>= sequence . map kvToRSE) >>= return . map fst

> waitForCalls :: Chan RString -> [RString] -> IO ()
> waitForCalls _ [] = return ()
> waitForCalls chan clients = do
>     receiverid <- readChan chan
>     if receiverid `elem` clients
>       then waitForCalls chan $ filter (/= receiverid) clients
>       else do
>         putStrLn $ "Ignoring unexpected response from " ++ show receiverid
>         waitForCalls chan clients

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

> makeServer :: Barrier -> MVar UTCTime -> Chan RString -> SAMPConnection -> IO ThreadId
> makeServer mv mt chan conn = do
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
>     forkIO (runServer mv mt chan sock pNum conn)

Creates the server, listening to the assigned socket/port.

> runServer :: Barrier -> MVar UTCTime -> Chan RString -> Socket -> PortNumber -> SAMPConnection -> IO ()
> runServer mv mt chan sock portNum conn = do
>      tid <- myThreadId
>      simpleHTTPWithSocket sock (nullConf { port = fromEnum portNum }) 
>          (handlers mv mt chan conn tid)

> handlers :: Barrier -> MVar UTCTime -> Chan RString -> SAMPConnection -> ThreadId -> ServerPart Response
> handlers mv mt chan conn tid = msum [handleXmlRpc mv mt chan conn tid, anyRequest (notFound (toResponse "unknown request"))]

XML-RPC is done via a HTML POST of an XML document.
At present I enforce the content-type restriction but
this may be a bit OTT.

> xmlct :: B.ByteString
> xmlct = B.pack "text/xml"

> handleXmlRpc :: Barrier -> MVar UTCTime -> Chan RString -> SAMPConnection -> ThreadId -> ServerPart Response
> handleXmlRpc mv mt chan conn tid = dir "xmlrpc" $ do
>     methodM POST
>     cType <- getHeaderM "content-type"
>     unless (cType == Just xmlct) $ escape (badRequest (toResponse ("Invalid content type: " ++ show cType)))
>     r <- askRq
>     getResponse mv mt chan conn tid (rqBody r)

> getResponse :: Barrier -> MVar UTCTime -> Chan RString -> SAMPConnection -> ThreadId -> RqBody -> ServerPart Response
> getResponse mv mt chan conn tid (Body bdy) = do
>     let call = L.unpack bdy
>     liftIO $ simpleClientServer conn (notifications tid) calls (rfunc mv mt chan) call
>     ok (toResponse "")

TODO: support hub shutdown

> allMT :: MType
> allMT = fromJust (toMType "*")

> notifications :: ThreadId -> [SAMPNotificationFunc]
> notifications tid =
>      [(allMT, handleOther)]

> calls :: [SAMPCallFunc]
> calls = []

> handleOther :: MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleOther mtype _ name keys = do
>     putStrLn $ "Notification of " ++ show mtype ++ " from " ++ show name
>     forM_ keys $ \(k,v) -> putStrLn $ "  " ++ show k ++ " -> " ++ showSAMPValue v
>     putStrLn ""

TODO: check that msgid is correct, which means it has to be sent in!

> rfunc :: Barrier -> MVar UTCTime -> Chan RString -> SAMPResponseFunc
> rfunc mv mt chan _ receiverid msgid rsp = do
>    eTime <- getCurrentTime
>    sTime <- readMVar mt
>    printResponse mv (diffUTCTime eTime sTime) receiverid rsp
>    writeChan chan receiverid


