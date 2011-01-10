
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
>         Right x -> do
>                      conn <- doE createClient
>
>                      -- the assumption is that we only need to unregister if we get
>                      -- a user interrupt but not for other errors.
>                      let cleanUp :: CE.AsyncException -> IO ()
>                          cleanUp e = when (e == CE.UserInterrupt) (doE (unregisterE conn)) >>
>                                      hPutStrLn stderr ("ERROR: " ++ show e) >> exitFailure
>     
>                      processMessage conn x `CE.catch` cleanUp
>                      doE (unregisterE conn)
>                      exitSuccess

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
>           case cmode of
>             Sync   -> newMVar () >>= \mv -> parallel_ (map (sendSync mv conn msg . fst) targets) >> stopGlobalPool
>             -- TODO: need to create the callback server
>             ASync  -> makeServer conn >> parallel_ (map (sendASync conn msg . fst) targets) >> stopGlobalPool
>             Notify -> putStrLn "Notifications sent to:" >> runE (notifyAllE conn msg) >>=
>                       mapM_ (\n -> putStrLn ("    " ++ show n))

TODO:

could report response time

> printResponse :: MVar () -> RString -> SAMPResponse -> IO ()
> printResponse mv target rsp = 
>     if isSAMPErrorOnly rsp
>         then printError mv target rsp
>         else if isSAMPWarning rsp
>           then printWarning mv target rsp
>           else printSuccess mv target rsp

> syncAction :: MVar () -> IO () -> IO ()
> syncAction mv act = modifyMVar mv $ \_ -> act >> return ((), ())

> printSuccess :: MVar () -> RString -> SAMPResponse -> IO ()
> printSuccess mv target rsp = syncAction mv $ do
>     putStrLn $ "Successful response from " ++ show target
>     let Just svals = getSAMPResponseResult rsp
>     forM_ svals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v

> printError :: MVar () -> RString -> SAMPResponse -> IO ()
> printError mv target rsp = syncAction mv $ do
>     putStrLn $ "Error response from " ++ show target
>     let Just (emsg, evals) = getSAMPResponseError rsp
>     putStrLn $ "    " ++ show emsg
>     forM_ evals $ \(k,v) -> putStrLn $ "    " ++ show k ++ " ->  " ++ show v

> printWarning :: MVar () -> RString -> SAMPResponse -> IO ()
> printWarning mv target rsp = syncAction mv $ do
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

> sendASync :: SAMPConnection -> SAMPMessage -> RString -> IO ()
> sendASync conn msg target = 
>     runE (callE conn target msgId msg) >> return ()

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

> makeServer :: SAMPConnection -> IO ThreadId
> makeServer conn = do
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
>     forkIO (runServer sock pNum conn)

Creates the server, listening to the assigned socket/port.

> runServer :: Socket -> PortNumber -> SAMPConnection -> IO ()
> runServer sock portNum conn = do
>      tid <- myThreadId
>      simpleHTTPWithSocket sock (nullConf { port = fromEnum portNum }) 
>          (handlers conn tid)

> handlers :: SAMPConnection -> ThreadId -> ServerPart Response
> handlers conn tid = msum [handleXmlRpc conn tid, anyRequest (notFound (toResponse "unknown request"))]

XML-RPC is done via a HTML POST of an XML document.
At present I enforce the content-type restriction but
this may be a bit OTT.

> xmlct :: B.ByteString
> xmlct = B.pack "text/xml"

> handleXmlRpc :: SAMPConnection -> ThreadId -> ServerPart Response
> handleXmlRpc conn tid = dir "xmlrpc" $ do
>     methodM POST
>     cType <- getHeaderM "content-type"
>     unless (cType == Just xmlct) $ escape (badRequest (toResponse ("Invalid content type: " ++ show cType)))
>     r <- askRq
>     getResponse conn tid (rqBody r)

> getResponse :: SAMPConnection -> ThreadId -> RqBody -> ServerPart Response
> getResponse conn tid (Body bdy) = do
>     let call = L.unpack bdy
>     liftIO $ simpleClientServer conn (notifications tid) calls rfunc call
>     ok (toResponse "")

> allMT :: MType
> allMT = fromJust (toMType "*")

> notifications :: ThreadId -> [SAMPNotificationFunc]
> notifications tid =
>      [(allMT, handleOther)]

> calls :: [SAMPCallFunc]
> calls = [(allMT, handleOtherCall)]

> handleOther :: MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleOther mtype _ name keys = do
>     putStrLn $ "Notification of " ++ show mtype ++ " from " ++ show name
>     forM_ keys $ \(k,v) -> putStrLn $ "  " ++ show k ++ " -> " ++ showSAMPValue v
>     putStrLn ""

> handleOtherCall :: MType -> RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse
> handleOtherCall mtype _ name _ keys = do
>     putStrLn $ "Call of " ++ show mtype ++ " by " ++ show name
>     forM_ keys $ \(k,v) -> putStrLn $ "  " ++ show k ++ " -> " ++ showSAMPValue v
>     putStrLn ""
>     let emsg = fromJust $ toRString $ "The message " ++ show mtype ++ " has only been logged, not acted on."
>     return $ toSAMPResponseWarning [] emsg []

Not sure what to do about this at the moment.

> rfunc :: SAMPResponseFunc
> rfunc _ receiverid msgid rsp =
>    if isSAMPSuccess rsp
>      then do
>             putStrLn $ "Got a response to msg=" ++ show msgid ++ " from=" ++ show receiverid
>             return ()
>      else putStrLn $ "ERROR: " ++ show (fromJust (getSAMPResponseErrorTxt rsp))

