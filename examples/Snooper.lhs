
ghc -hide-package monads-fd --make -o snooper -Wall Snooper.lhs -i..

Try and log messages sent from the SAMP hub.

Usage:

   ./snooper

TODO:

  - since we know when clients register/unregister from the hub
    and any metadata they assert, we can keep a mapping from id to
    samp.name, so this can be used in reports.

    Should we auto-convert the id parameter of messages to
    include this information?

  - most (if not all) messages will have an id field in the parameter,
    so we should recognize this and extract/display it. Which means
    some helper routines.

  - look at error handling

> module Main where
>
> import System.Environment (getArgs, getProgName)
> import System.Exit (exitSuccess, exitFailure)
> import System.IO
> import System.IO.Error
>
> import qualified Control.Exception as CE
> import Control.Concurrent (ThreadId, killThread, myThreadId, forkIO)
> import Control.Concurrent.MVar
>
> import Control.Monad (msum, unless, when)
> import Control.Monad.Trans (liftIO)
>
> import Data.Maybe (fromJust, fromMaybe)
> import qualified Data.Map as M
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
> mdataMT , registerMT, unregisterMT, pingMT , shutdownMT , allMT :: MType
> mdataMT = fromJust (toMType "samp.hub.event.metadata")
> registerMT = fromJust (toMType "samp.hub.event.register")
> unregisterMT = fromJust (toMType "samp.hub.event.unregister")
> pingMT = fromJust (toMType "samp.app.ping")
> shutdownMT = fromJust (toMType "samp.hub.event.shutdown")
> allMT = fromJust (toMType "*")

> sName , idLabel , mdataLabel :: RString
> sName = fromJust (toRString "samp.name")
> idLabel = fromJust (toRString "id")
> mdataLabel = fromJust (toRString "metadata")

Set up a simple client (i.e. with limited metadata)

> authorMetadata :: Err IO [SAMPKeyValue]
> authorMetadata = mapM (uncurry stringToKeyValE)
>     [("author.name", "Doug Burke"),
>      ("author.affiliation", "Smithsonian Astrophysical Observatory"),
>      ("author.mail", "dburke@cfa.harvard.edu")]

> createClient :: Err IO SAMPConnection
> createClient =
>      authorMetadata >>= \amd ->
>      getHubInfoE >>=
>      registerClientE >>= \conn ->
>      toMetadataE "hsamp-snooper" (Just "Report on messages sent by the hub.")
>          Nothing Nothing Nothing >>= \md ->
>      declareMetadataE conn (md ++ amd) >>
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

Try and ensure sequential output to the screen. Given that we want to respond
to the hub and display a message it may be better to use a Channel to send
the screen output to a separate display thread (to try and avoid any waiting for the
barrier)?

> type Barrier = MVar ()

> newBarrier :: IO Barrier
> newBarrier = newMVar ()

Displays the given string to stdout, ensuring that it is done as an
atomic operation (as long as other displays are also done with
syncPrint_ using the same barrier). The print occurs in a separate
thread so that if there is a delay due to the barrier being used
elsewhere it won't affect the calling routine.

> syncPrint_ :: Barrier -> String -> IO ()
> syncPrint_ barrier msg = forkIO (withMVar barrier $ \_ -> putStrLn msg) >> return ()

The client map is a mapping from the client Id (identifies the
SAMP client and is created by the hub) with a user friendly
string, which is a combination of the id and the samp.name
setting of the client (if defined).

> type ClientMap = M.Map RString String
> type ClientMapVar = MVar ClientMap

> bracket :: RString -> String
> bracket a = " (" ++ fromRString a ++ ")"

> toClientName :: RString -> Maybe RString -> String
> toClientName k v = fromRString k ++ fromMaybe "" (fmap bracket v)

TODO: can we use M.fromListWithKey here?

> newClientMap :: SAMPConnection -> IO ClientMapVar
> newClientMap conn = do
>     clients <- runE (getClientNamesE conn)
>     let conv (k,v) = (k, toClientName k v)
>     newMVar $ M.fromList $ map conv clients

> getFromMap :: ClientMap -> RString -> String
> getFromMap clmap clid = M.findWithDefault (fromRString clid) clid clmap

Get the display name for a client

> getDisplayName :: ClientMapVar -> RString -> IO String
> getDisplayName clvar clid = do
>     clmap <- readMVar clvar
>     return $ getFromMap clmap clid

Remove the client from the map, returning its display name

> removeClient :: ClientMapVar -> RString -> IO String
> removeClient clvar clid = 
>     modifyMVar clvar $ \clmap -> do
>         let rval = getFromMap clmap clid
>         return (M.delete clid clmap, rval)

Add the client to the map, returning its display name. Will overwrite
existing values.

> addClient :: ClientMapVar -> RString -> Maybe RString -> IO String
> addClient clvar clid clname = do
>     let name = toClientName clid clname
>     modifyMVar clvar $ \clmap ->
>         return (M.insert clid name clmap, name)

Creates the server, listening to the assigned socket/port.

> runServer :: Socket -> PortNumber -> SAMPConnection -> IO ()
> runServer sock portNum conn = do
>      tid <- myThreadId
>      barrier <- newBarrier
>      clvar <- newClientMap conn
>      simpleHTTPWithSocket sock (nullConf { port = fromEnum portNum }) 
>          (handlers conn tid barrier clvar)

> handlers :: SAMPConnection -> ThreadId -> Barrier -> ClientMapVar -> ServerPart Response
> handlers conn tid barrier clvar =
>     msum [handleXmlRpc conn tid barrier clvar, anyRequest (notFound (toResponse "unknown request"))]

XML-RPC is done via a HTML POST of an XML document.
At present I enforce the content-type restriction but
this may be a bit OTT.

> xmlct :: B.ByteString
> xmlct = B.pack "text/xml"

> handleXmlRpc :: SAMPConnection -> ThreadId -> Barrier -> ClientMapVar -> ServerPart Response
> handleXmlRpc conn tid barrier clvar = dir "xmlrpc" $ do
>     methodM POST
>     cType <- getHeaderM "content-type"
>     unless (cType == Just xmlct) $ escape (badRequest (toResponse ("Invalid content type: " ++ show cType)))
>     r <- askRq
>     getResponse conn tid barrier clvar (rqBody r)

> getResponse :: SAMPConnection -> ThreadId -> Barrier -> ClientMapVar -> RqBody -> ServerPart Response
> getResponse conn tid barrier clvar (Body bdy) = do
>     let call = L.unpack bdy
>     liftIO $ simpleClientServer conn (notifications tid barrier clvar) (calls barrier clvar) (rfunc barrier clvar) call
>     ok (toResponse "")

> notifications :: ThreadId -> Barrier -> ClientMapVar -> [SAMPNotificationFunc]
> notifications tid barrier clvar =
>      [(registerMT, handleRegister barrier clvar),
>       (unregisterMT, handleUnregister barrier clvar),
>       (mdataMT, handleMetadata barrier clvar),
>       (shutdownMT, handleShutdown tid),
>       (allMT, handleOther barrier clvar)
>      ]

> calls :: Barrier -> ClientMapVar -> [SAMPCallFunc]
> calls barrier clvar = [(pingMT, handlePingCall barrier clvar), (allMT, handleOtherCall barrier clvar)]

> handleShutdown :: ThreadId -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleShutdown tid _ _ _ _ = killThread tid

> displayKV :: SAMPKeyValue -> String
> displayKV (k,v) = "  " ++ fromRString k ++ " -> " ++ showSAMPValue v

> showKV :: SAMPKeyValue -> IO ()
> showKV = putStrLn . displayKV

> getKeyStr :: [SAMPKeyValue] -> RString -> Maybe RString
> getKeyStr kvs key =
>     case lookup key kvs of
>         Just (SAMPString s) -> Just s
>         _ -> Nothing

Return the value of the key from the input list, along with the
remaining key,value pairs.

> getKeyVal :: ([SAMPKeyValue] -> RString -> Maybe a) -> [SAMPKeyValue] -> RString -> Maybe (a, [SAMPKeyValue])
> getKeyVal get kvs key = 
>     let f a = (a, filter ((/= key) . fst) kvs)
>     in fmap f (get kvs key)

look for id field in the params and remove it. for specific mtypes we know
the other required/suggested keys too.

> maybeWithLabel :: ([SAMPKeyValue] -> RString -> Maybe a) ->
>                   RString -> [SAMPKeyValue] -> (a -> [SAMPKeyValue] -> IO ()) -> IO () -> IO ()
> maybeWithLabel get lbl keys hasLbl noLbl  = 
>    case getKeyVal get keys lbl of
>        Nothing -> noLbl
>        Just (clid,kvs) -> hasLbl clid kvs

> maybeWithId :: [SAMPKeyValue] -> (RString -> [SAMPKeyValue] -> IO ()) -> IO () -> IO ()
> maybeWithId = maybeWithLabel getKeyStr idLabel

> displayWithKeys :: [String] -> [SAMPKeyValue] -> [String] -> String
> displayWithKeys hdr keys footer = unlines (hdr ++ map displayKV keys ++ footer)

> withId :: String -> Barrier -> [SAMPKeyValue] -> (RString -> [SAMPKeyValue] -> IO ()) -> IO ()
> withId lbl barrier keys hasId = 
>    let noId = syncPrint_ barrier $ displayWithKeys
>                   ["ERROR: unable to find id in parameters of " ++ lbl ++ " call"] keys []
>    in maybeWithId keys hasId noId

> displayMsgId :: RString -> String
> displayMsgId msgid = "  Message id: " ++ fromRString msgid

> handleRegister :: Barrier -> ClientMapVar -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleRegister barrier clvar _ _ name keys = 
>     withId "registration" barrier keys $ \clid kvs -> do
>         clname <- addClient clvar clid Nothing
>         syncPrint_ barrier $ displayWithKeys
>             ["Client has added itself to " ++ fromRString name ++ ": " ++ clname] kvs []

> handleUnregister :: Barrier -> ClientMapVar -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleUnregister barrier clvar _ _ name keys = 
>     withId "unregistration" barrier keys $ \clid kvs -> do
>         clname <- removeClient clvar clid
>         syncPrint_ barrier $ displayWithKeys
>             ["Client has removed itself from " ++ fromRString name ++ ": " ++ clname] kvs []

TODO: handleSubscriptions

> handleMetadata :: Barrier -> ClientMapVar -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleMetadata barrier clvar _ _ name keys = 
>     withId "metadata" barrier keys $ \clid kvs -> do
>         oclname <- getDisplayName clvar clid
>         let doIt mdata k2 = do
>               case mdata of
>                   SAMPMap mds -> do
>                                    nclname <- addClient clvar clid $ getKeyStr mds sName
>                                    let clname = oclname ++ if oclname == nclname then "" else " -> " ++ nclname
>                                    syncPrint_ barrier $ displayWithKeys
>                                        ["Metadata notification from " ++ fromRString name ++ " for " ++ clname]
>                                        mds (if null k2 then [] else " Other arguments:" : map displayKV k2)
>
>                   _ -> syncPrint_ barrier $ displayWithKeys
>                          ["Metadata notification from " ++ fromRString name ++ " for " ++ oclname,
>                          "  ERROR Expected metadata to be a map!"] kvs []
>
>             failIt = syncPrint_ barrier $ displayWithKeys
>                        ["Metadata notification from " ++ fromRString name ++ " for " ++ oclname,
>                        "  ERROR missing metadata parameter"] kvs []
>
>         maybeWithLabel (flip lookup) mdataLabel kvs doIt failIt

> handleOther :: Barrier -> ClientMapVar -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleOther barrier clvar mtype msgid name keys = 
>     let noId = do
>             clname <- getDisplayName clvar name
>             syncPrint_ barrier $ displayWithKeys
>                 ["Notification of " ++ show mtype ++ " from " ++ clname,
>                  displayMsgId msgid] keys []
>         
>         hasId clid kvs = do
>             clname <- getDisplayName clvar clid
>             syncPrint_ barrier $ displayWithKeys
>                 ["Notification of " ++ show mtype ++ " from " ++ fromRString name ++ " for " ++ clname,
>                  displayMsgId msgid] kvs []
>
>     in maybeWithId keys hasId noId

> handlePingCall :: Barrier -> ClientMapVar -> MType -> RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse
> handlePingCall barrier clvar _ _ name _ keys = do
>     clname <- getDisplayName clvar name
>     syncPrint_ barrier $ displayWithKeys
>       ["hsamp-snooper was pinged by " ++ clname] keys []
>     return $ toSAMPResponse []

We return a warning to point out that we are just logging this message
(basically copying the behavior of Mark's snooper here).

> handleOtherCall :: Barrier -> ClientMapVar -> MType -> RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse
> handleOtherCall barrier clvar mtype _ name msgid keys = do
>     clname <- getDisplayName clvar name
>     syncPrint_ barrier $ displayWithKeys
>       ["Call of " ++ show mtype ++ " by " ++ clname, displayMsgId msgid] keys []
>     let emsg = fromJust $ toRString $ "The message " ++ show mtype ++ " has only been logged, not acted on."
>     return $ toSAMPResponseWarning [] emsg []

Not sure what to do about this at the moment.

> rfunc :: Barrier -> ClientMapVar -> SAMPResponseFunc
> rfunc barrier clvar _ clid msgid rsp = do
>    clname <- getDisplayName clvar clid
>    let msg = if isSAMPSuccess rsp
>                then "Got a response to msg=" ++ fromRString msgid ++ " from=" ++ clname
>                else unlines ["Error in response to msg=" ++ fromRString msgid ++ " from=" ++ clname,
>                              show (fromJust (getSAMPResponseErrorTxt rsp))]
>    syncPrint_ barrier msg

