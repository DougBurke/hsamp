
ghc -hide-package monads-fd --make -o sender -Wall Sender.lhs -i..

Send a SAMP message (with potential arguments) to all interested clients.

Usage:

   ./sender [sync|async|notify] mtype [param1=value1] [param2=value2] ... [paramN=valueN]

TODO:

  - target name(s)
  - sendername/metadata

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
> import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
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
>               -- the assumption is that we only need to unregister if we get
>               -- a user interrupt but not for other errors.
>               let cleanUp :: CE.AsyncException -> IO ()
>                   cleanUp e = when (e == CE.UserInterrupt) (doE (unregisterE conn)) >>
>                               hPutStrLn stderr ("ERROR: " ++ show e) >> exitFailure
>     
>               processMessage conn x `CE.catch` cleanUp
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
>             ASync  -> newEmptyMVar >>= \amv -> makeServer amv conn >> sendASync amv conn msg >> stopGlobalPool
>             Notify -> putStrLn "Notifications sent to:" >> runE (notifyAllE conn msg) >>=
>                       mapM_ (\n -> putStrLn ("    " ++ show n))

> type AMVar = (UTCTime, [(RString, RString)])

TODO:

could report response time

> printResponse :: MVar () -> RString -> SAMPResponse -> IO ()
> printResponse mv target rsp = 
>     if isSAMPErrorOnly rsp
>         then syncAction mv $ printError target rsp
>         else if isSAMPWarning rsp
>           then syncAction mv $ printWarning target rsp
>           else syncAction mv $ printSuccess target rsp

> syncAction :: MVar () -> IO () -> IO ()
> syncAction mv act = withMVar mv $ \_ -> act

> printSuccess :: RString -> SAMPResponse -> IO ()
> printSuccess target rsp = do
>     putStrLn $ "Successful response from " ++ show target
>     let Just svals = getSAMPResponseResult rsp
>     forM_ svals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v

> printError :: RString -> SAMPResponse -> IO ()
> printError target rsp = do
>     putStrLn $ "Error response from " ++ show target
>     let Just (emsg, evals) = getSAMPResponseError rsp
>     putStrLn $ "    " ++ show emsg
>     forM_ evals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v

> printWarning :: RString -> SAMPResponse -> IO ()
> printWarning target rsp = do
>     putStrLn $ "Warning response from " ++ show target
>     let Just svals = getSAMPResponseResult rsp
>         Just (emsg, evals) = getSAMPResponseError rsp
>     putStrLn $ "    " ++ show emsg
>     unless (null svals) $ do
>         putStrLn $ "  Response"
>         forM_ svals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v
>     unless (null evals) $ do
>         putStrLn $ "  Error"
>         forM_ evals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v

> sendSync :: MVar () -> SAMPConnection -> SAMPMessage -> RString -> IO ()
> sendSync mv conn msg target = 
>     runE (callAndWaitE conn target msg (Just 10)) >>= printResponse mv target

Hardly unique!

> msgId :: RString
> msgId = fromJust (toRString "unique-identifier-string-honest!")

> kvToRSE :: (Monad m) => SAMPKeyValue -> Err m (RString, RString)
> kvToRSE (k,SAMPString v) = return (k, v)
> kvToRSE (k,v) = fail $ "Key " ++ show k ++ " should be a SAMP string but found " ++ show v

TODO: need to handle errors more sensibly than runE here!

> sendASync :: MVar AMVar -> SAMPConnection -> SAMPMessage -> IO ()
> sendASync amv conn msg = do
>     sTime <- getCurrentTime
>     runE (callAllE conn msgId msg >>= sequence . map kvToRSE >>= liftIO . putMVar amv . ((,) sTime))

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

TODO: will need some what of keeping the server alive until it has
received responses from all contacted clients (or timed out).

> makeServer :: MVar AMVar -> SAMPConnection -> IO ThreadId
> makeServer amv conn = do
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
>     forkIO (runServer amv sock pNum conn)

Creates the server, listening to the assigned socket/port.

> runServer :: MVar AMVar -> Socket -> PortNumber -> SAMPConnection -> IO ()
> runServer amv sock portNum conn = do
>      tid <- myThreadId
>      simpleHTTPWithSocket sock (nullConf { port = fromEnum portNum }) 
>          (handlers amv conn tid)

> handlers :: MVar AMVar -> SAMPConnection -> ThreadId -> ServerPart Response
> handlers amv conn tid = msum [handleXmlRpc amv conn tid, anyRequest (notFound (toResponse "unknown request"))]

XML-RPC is done via a HTML POST of an XML document.
At present I enforce the content-type restriction but
this may be a bit OTT.

> xmlct :: B.ByteString
> xmlct = B.pack "text/xml"

> handleXmlRpc :: MVar AMVar -> SAMPConnection -> ThreadId -> ServerPart Response
> handleXmlRpc amv conn tid = dir "xmlrpc" $ do
>     methodM POST
>     cType <- getHeaderM "content-type"
>     unless (cType == Just xmlct) $ escape (badRequest (toResponse ("Invalid content type: " ++ show cType)))
>     r <- askRq
>     getResponse amv conn tid (rqBody r)

> getResponse :: MVar AMVar -> SAMPConnection -> ThreadId -> RqBody -> ServerPart Response
> getResponse amv conn tid (Body bdy) = do
>     let call = L.unpack bdy
>     liftIO $ simpleClientServer conn (notifications tid) calls (rfunc amv) call
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

TODO: check for client/msg id in clients list.

> rfunc :: MVar AMVar -> SAMPResponseFunc
> rfunc amv _ receiverid msgid rsp = do
>    eTime <- getCurrentTime
>    (sTime, clients) <- readMVar amv
>    if isSAMPSuccess rsp
>      then do
>             let delta = diffUTCTime eTime sTime
>             putStrLn $ "Response from " ++ show receiverid ++ " took " ++ show delta ++ " seconds."
>             return ()
>      else putStrLn $ "ERROR: " ++ show (fromJust (getSAMPResponseErrorTxt rsp))

