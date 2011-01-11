
ghc -hide-package monads-fd --make -o snooper -Wall Snooper.lhs

Try and log messages sent from the SAMP hub.

Usage:

   ./snooper

TODO:

  - since we know when clients register/unregister from the hub
    and any metadata they assert, we can keep a mapping from id to
    samp.name, so this can be used in reports.

  - look at error handling

> module Main where
>
> import System.Environment (getArgs, getProgName)
> import System.Exit (exitSuccess, exitFailure)
> import System.IO
> import System.IO.Error
>
> import qualified Control.Exception as CE
> import Control.Concurrent (ThreadId, killThread, myThreadId)
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
> usage = getProgName >>= \n -> hPutStrLn stderr $ "Usage: " ++ n ++ " [1]\n\nSupplying a 1 means to display debugging messages.\n\n"
>
> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         ["1"] -> updateGlobalLogger "SAMP" (setLevel DEBUG)
>         [] -> return ()
>         _ -> usage >> exitFailure
>
>     -- is this excessive?
>     let ioHdlr :: CE.IOException -> IO ()
>         ioHdlr e = let emsg = if isUserError e then ioeGetErrorString e else show e
>                    in putStrLn ("ERROR: " ++ emsg) >> exitFailure
>
>         -- this assumes the only way to get a ThreadKilled is via a shutdown message
>         asyncHdlr :: CE.AsyncException -> IO ()
>         asyncHdlr e = let emsg = if e == CE.ThreadKilled then "SAMP Hub has shut down." else show e
>                       in putStrLn ("ERROR: " ++ emsg) >> exitFailure
>
>         otherHdlr :: CE.SomeException -> IO ()
>         otherHdlr e = putStrLn ("ERROR: " ++ show e) >> exitFailure
>
>     snoop `CE.catches` [CE.Handler ioHdlr, CE.Handler asyncHdlr, CE.Handler otherHdlr]
>     exitSuccess
>
> mdataMT , pingMT , shutdownMT , allMT :: MType
> mdataMT = fromJust (toMType "samp.hub.event.metadata")
> pingMT = fromJust (toMType "samp.app.ping")
> shutdownMT = fromJust (toMType "samp.hub.event.shutdown")
> allMT = fromJust (toMType "*")

Set up a simple client (i.e. with limited metadata)

> createClient :: Err IO SAMPConnection
> createClient =
>      getHubInfoE >>=
>      registerClientE >>= \conn ->
>      toMetadataE "snooper" (Just "Report on messages sent by the hub.")
>          Nothing Nothing Nothing >>=
>      declareMetadataE conn >>
>      return conn

Basic configuration for setting up the server.

> eConf :: Conf
> eConf = nullConf { port = 0 }

The location of the server we create to handle incoming messages from
the SAMP hub. Note that this includes the XML-RPC route since in this
case we do not support any other access.

> hostName :: PortNumber -> String
> hostName portNum = "http://127.0.0.1:" ++ show portNum ++ "/xmlrpc"

We need to create a server to service messages from the Hub
as well as the SAMP client. We don't actually start the server
up until after registering for the calls, which means there
is a small window when we won't receive messages, but I am
willing to live with this for now.

> snoop :: IO ()
> snoop = do
>     -- create the client
>     conn <- runE createClient
>
>     -- the assumption is that we only need to unregister if we get
>     -- a user interrupt but not for other errors.
>     let cleanUp :: CE.AsyncException -> IO ()
>         cleanUp e = when (e == CE.UserInterrupt) (runE (unregisterE conn)) >> CE.throwIO e
>     
>     runSnooper conn `CE.catch` cleanUp

Set up the subscriptions and run the server to process the requests.

> runSnooper :: SAMPConnection -> IO ()
> runSnooper conn = do
>     -- what is the address of the server?
>     sock <- bindPort eConf
>     pNum <- socketPort sock
>     url <- runE $ toRStringE (hostName pNum)
>
>     -- register the messages that we are interested in
>     runE $ setXmlrpcCallbackE conn url >>
>            declareSubscriptionsSimpleE conn [allMT]
>
>     -- now run the server
>     runServer sock pNum conn

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

I am not entirely convinced I am responding correctly here (the fact we
respond with an empty string).

> getResponse :: SAMPConnection -> ThreadId -> RqBody -> ServerPart Response
> getResponse conn tid (Body bdy) = do
>     let call = L.unpack bdy
>     liftIO $ simpleClientServer conn (notifications tid) calls rfunc call
>     ok (toResponse "")

> notifications :: ThreadId -> [SAMPNotificationFunc]
> notifications tid =
>      [(shutdownMT, handleShutdown tid),
>       (mdataMT, handleMetadata),
>       (allMT, handleOther)
>      ]

> calls :: [SAMPCallFunc]
> calls = [(pingMT, handlePingCall), (allMT, handleOtherCall)]

> handleShutdown :: ThreadId -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleShutdown tid _ _ _ _ = killThread tid

> showKV :: SAMPKeyValue -> IO ()
> showKV (k,v) = putStrLn $ "  " ++ fromRString k ++ " -> " ++ showSAMPValue v

> handleMetadata :: MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleMetadata _ _ name keys = do
>     putStrLn $ "Metadata notification from " ++ fromRString name
>     forM_ keys showKV
>     putStrLn ""

> handleOther :: MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleOther mtype _ name keys = do
>     putStrLn $ "Notification of " ++ show mtype ++ " from " ++ fromRString name
>     forM_ keys showKV
>     putStrLn ""

> handlePingCall :: MType -> RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse
> handlePingCall _ _ name _ keys = do
>     putStrLn $ "Snooper was pinged by " ++ fromRString name
>     forM_ keys showKV
>     putStrLn ""
>     return $ toSAMPResponse []

We return a warning to point out that we are just logging this message
(basically copying the behavior of Mark's snooper here).

> handleOtherCall :: MType -> RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse
> handleOtherCall mtype _ name _ keys = do
>     putStrLn $ "Call of " ++ show mtype ++ " by " ++ fromRString name
>     forM_ keys showKV
>     putStrLn ""
>     let emsg = fromJust $ toRString $ "The message " ++ show mtype ++ " has only been logged, not acted on."
>     return $ toSAMPResponseWarning [] emsg []

Not sure what to do about this at the moment.

> rfunc :: SAMPResponseFunc
> rfunc _ receiverid msgid rsp =
>    if isSAMPSuccess rsp
>      then do
>             putStrLn $ "Got a response to msg=" ++ fromRString msgid ++ " from=" ++ fromRString receiverid
>             return ()
>      else putStrLn $ "ERROR: " ++ show (fromJust (getSAMPResponseErrorTxt rsp))

