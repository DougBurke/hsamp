> {-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2011, 2013
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------

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
> import Control.Concurrent (ThreadId, killThread, myThreadId)
> import Control.Concurrent.MVar
>
> import Control.Monad (when)
>
> import Data.Maybe (fromJust, fromMaybe)
> import qualified Data.Map as M
>
> import System.Log.Logger
>
> import Network.SAMP.Standard
> import Network.SAMP.Standard.Server.Snap
>
> import qualified Network as N
> import Network.Socket (Socket)
>
> import Utils
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
>     withSAMP $ do
>     sock <- getSocket
>
>     -- is this excessive?
>     let closeSock = N.sClose sock
>          
>         ioHdlr :: CE.IOException -> IO ()
>         ioHdlr e = let emsg = if isUserError e then ioeGetErrorString e else show e
>                    in closeSock >> putStrLn ("ERROR: " ++ emsg) >> exitFailure
>
>         -- this assumes the only way to get a ThreadKilled is via a shutdown message
>         asyncHdlr :: CE.AsyncException -> IO ()
>         asyncHdlr e = let emsg = if e == CE.ThreadKilled then "SAMP Hub has shut down." else show e
>                       in closeSock >> putStrLn ("ERROR: " ++ emsg) >> exitFailure
>
>         otherHdlr :: CE.SomeException -> IO ()
>         otherHdlr e = closeSock >> putStrLn ("ERROR: " ++ show e) >> exitFailure
>
>     snoop sock `CE.catches` [CE.Handler ioHdlr, CE.Handler asyncHdlr, CE.Handler otherHdlr]
>     closeSock
>     exitSuccess
>
> sName :: RString
> sName = "samp.name"

We need to create a server to service messages from the Hub
as well as the SAMP client. We don't actually start the server
up until after registering for the calls, which means there
is a small window when we won't receive messages, but I am
willing to live with this for now.

> snoop :: Socket -> IO ()
> snoop sock = do
>     -- create the client
>     conn <- runE $ createClient "hsamp-snooper"
>                       "Report on messages sent by the hub."
>
>     -- the assumption is that we only need to unregister if we get
>     -- a user interrupt but not for other errors.
>     let cleanUp :: CE.AsyncException -> IO ()
>         cleanUp e = when (e == CE.UserInterrupt) (runE (unregisterE conn)) >> CE.throwIO e
>     
>     runSnooper sock conn `CE.catch` cleanUp

Set up the subscriptions and run the server to process the requests.

> runSnooper :: Socket -> SAMPConnection -> IO ()
> runSnooper sock conn = do
>     -- what is the address of the server?
>     url <- getAddress sock
>     urlR <- runE $ toRStringE url
>     putStrLn $ "Snooping using " ++ url ++ "\n"
>
>     -- register the messages that we are interested in
>     runE $ setXmlrpcCallbackE conn urlR >>
>            declareSubscriptionsSimpleE conn ["*"]
>
>     -- now run the server
>     tid <- myThreadId
>     (pchan, _) <- startPrintChannel
>     clvar <- newClientMap conn
>     runServer sock url $ processCall conn tid pchan clvar

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

> processCall :: SAMPConnection -> ThreadId -> PrintChannel -> ClientMapVar -> String -> IO ()
> processCall conn tid pchan clvar = 
>     simpleClientServer conn (notifications tid pchan clvar) (calls pchan clvar) (rfunc pchan clvar)

TODO: should we have a handler for samp.hub.event.* which then 

  - checks for id field
  - processes remaining contents

or at least abstract out this for the messages we do support.

> notifications :: ThreadId -> PrintChannel -> ClientMapVar -> [SAMPNotificationFunc]
> notifications tid pchan clvar =
>      [("samp.hub.event.register", handleRegister pchan clvar),
>       ("samp.hub.event.unregister", handleUnregister pchan clvar),
>       ("samp.hub.event.metadata", handleMetadata pchan clvar),
>       ("samp.hub.event.subscriptions", handleSubscriptions pchan clvar),
>       ("samp.hub.event.shutdown", handleShutdown tid),
>       ("*", handleOther pchan clvar)
>      ]

> calls :: PrintChannel -> ClientMapVar -> [SAMPCallFunc]
> calls pchan clvar =
>     [("samp.app.ping", handlePingCall pchan clvar),
>      ("*", handleOtherCall pchan clvar)]

> handleShutdown :: ThreadId -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleShutdown tid _ _ _ _ = killThread tid

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

Can the above be done something like fmap (uncurry hasLbl) ...

> maybeWithId :: [SAMPKeyValue] -> (RString -> [SAMPKeyValue] -> IO ()) -> IO () -> IO ()
> maybeWithId = maybeWithLabel getKeyStr "id"

Ensure we end with a blank line

> displayWithKeys :: [String] -> [SAMPKeyValue] -> [String] -> [String]
> displayWithKeys hdr keys footer = hdr ++ map displayKV keys ++ footer ++ [""]

> withId :: String -> PrintChannel -> [SAMPKeyValue] -> (RString -> [SAMPKeyValue] -> IO ()) -> IO ()
> withId lbl pchan keys hasId = 
>    let noId = syncPrint pchan $ displayWithKeys
>                   ["Hub event: " ++ lbl] keys []
>    in maybeWithId keys hasId noId

> displaySecret , displayMsgId :: RString -> String
> displaySecret = ("  Secret    : " ++) . fromRString
> displayMsgId  = ("  Message id: " ++) . fromRString

> handleRegister :: PrintChannel -> ClientMapVar -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleRegister pchan clvar _ _ name keys = 
>     withId "registration" pchan keys $ \clid kvs -> do
>         clname <- addClient clvar clid Nothing
>         syncPrint pchan $ displayWithKeys
>             ["Client has added itself to " ++ fromRString name ++ ": " ++ clname] kvs []

> handleUnregister :: PrintChannel -> ClientMapVar -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleUnregister pchan clvar _ _ name keys = 
>     withId "unregistration" pchan keys $ \clid kvs -> do
>         clname <- removeClient clvar clid
>         syncPrint pchan $ displayWithKeys
>             ["Client has removed itself from " ++ fromRString name ++ ": " ++ clname] kvs []

> handleMetadata :: PrintChannel -> ClientMapVar -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleMetadata pchan clvar _ _ name keys = 
>     withId "metadata" pchan keys $ \clid kvs -> do
>         oclname <- getDisplayName clvar clid
>         let doIt mdata k2 = 
>               case mdata of
>                   SAMPMap mds -> do
>                                    nclname <- addClient clvar clid $ getKeyStr mds sName
>                                    let clname = oclname ++ if oclname == nclname then "" else " -> " ++ nclname
>                                    syncPrint pchan $ displayWithKeys
>                                        ["Metadata notification from " ++ fromRString name ++ " for " ++ clname]
>                                        mds (if null k2 then [] else " Other arguments:" : map displayKV k2)
>
>                   _ -> syncPrint pchan $ displayWithKeys
>                          ["Metadata notification from " ++ fromRString name ++ " for " ++ oclname,
>                          "  ERROR Expected metadata to be a map!"] kvs []
>
>             failIt = syncPrint pchan $ displayWithKeys
>                        ["Metadata notification from " ++ fromRString name ++ " for " ++ oclname,
>                        "  ERROR missing metadata parameter"] kvs []
>
>         maybeWithLabel (flip lookup) "metadata" kvs doIt failIt

> handleSubscriptions :: PrintChannel -> ClientMapVar -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleSubscriptions pchan clvar _ _ name keys = 
>     withId "subscriptions" pchan keys $ \clid kvs -> do
>         clname <- getDisplayName clvar clid
>         let doIt subs k2 = 
>               case subs of
>                   SAMPMap sds -> syncPrint pchan $ displayWithKeys
>                                      ["Subscriptions notification from " ++ fromRString name ++ " for " ++ clname]
>                                      sds (if null k2 then [] else " Other arguments:" : map displayKV k2)
>
>                   _ -> syncPrint pchan $ displayWithKeys
>                          ["Subscriptions notification from " ++ fromRString name ++ " for " ++ clname,
>                          "  ERROR Expected subscriptions to be a map!"] kvs []
>
>             failIt = syncPrint pchan $ displayWithKeys
>                        ["Subscriptions notification from " ++ fromRString name ++ " for " ++ clname,
>                        "  ERROR missing subscriptions parameter"] kvs []
>
>         maybeWithLabel (flip lookup) "subscriptions" kvs doIt failIt

> handleOther :: PrintChannel -> ClientMapVar -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
> handleOther pchan clvar mtype msgid name keys = 
>     let noId = do
>             clname <- getDisplayName clvar name
>             syncPrint pchan $ displayWithKeys
>                 ["Notification of " ++ show mtype ++ " from " ++ clname,
>                  displayMsgId msgid] keys []
>         
>         hasId clid kvs = do
>             clname <- getDisplayName clvar clid
>             syncPrint pchan $ displayWithKeys
>                 ["Notification of " ++ show mtype ++ " from " ++ fromRString name ++ " for " ++ clname,
>                  displayMsgId msgid] kvs []
>
>     in maybeWithId keys hasId noId

> handlePingCall :: PrintChannel -> ClientMapVar -> MType -> RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse
> handlePingCall pchan clvar _ secret name msgid keys = do
>     clname <- getDisplayName clvar name
>     syncPrint pchan $ displayWithKeys
>       ["hsamp-snooper was pinged by " ++ clname, displayMsgId msgid, displaySecret secret] keys []
>     return $ toSAMPResponse []

We return a warning to point out that we are just logging this message
(basically copying the behavior of Mark's snooper here).

> handleOtherCall :: PrintChannel -> ClientMapVar -> MType -> RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse
> handleOtherCall pchan clvar mtype secret name msgid keys = do
>     clname <- getDisplayName clvar name
>     syncPrint pchan $ displayWithKeys
>       ["Call of " ++ show mtype ++ " by " ++ clname, displayMsgId msgid, displaySecret secret] keys []
>     let emsg = fromJust $ toRString $ "The message " ++ show mtype ++ " has only been logged, not acted on."
>     return $ toSAMPResponseWarning [] emsg []

TODO: handle warning case, although when is this ever called?

> rfunc :: PrintChannel -> ClientMapVar -> SAMPResponseFunc
> rfunc pchan clvar secret clid msgid rsp = do
>    clname <- getDisplayName clvar clid
>    let msg = if isSAMPSuccess rsp
>                then ["Got a response to msg=" ++ fromRString msgid ++ " from=" ++ clname,
>                      displaySecret secret]
>                else ["Error in response to msg=" ++ fromRString msgid ++ " from=" ++ clname,
>                      show (fromJust (getSAMPResponseErrorTxt rsp))]
>    syncPrint pchan msg

