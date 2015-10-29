{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2011, 2013, 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------
-}

{-

Try and log messages sent from the SAMP hub.

Usage:

   ./snooper --debug

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

-}

module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isUserError, ioeGetErrorString)

import qualified Control.Exception as CE
import Control.Concurrent (ThreadId, killThread, myThreadId)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)

-- import Control.Monad (when)

import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import System.Log.Logger (Priority(DEBUG), setLevel, updateGlobalLogger)

import Network.SAMP.Standard (RString, SAMPKeyValue, SAMPValue(..)
                             , SAMPResponse, SAMPResponseFunc
                             , MType, MessageId
                             , ClientName, ClientSecret
                             , SAMPCallFunc, SAMPConnection
                             , SAMPNotificationFunc
                             , withSAMP
                             , unregisterE
                             , isSAMPSuccess
                             , setXmlrpcCallbackE
                             , getSAMPResponseErrorTxt
                             , declareSubscriptionsSimpleE
                             , runE, getClientNamesE
                             , toSAMPResponse, toSAMPResponseWarning
                             , fromRString, toRString, toRStringE
                             , fromMessageId
                             , toClientName
                             , fromClientSecret
                             , simpleClientServer)
import Network.SAMP.Standard.Server.Scotty (runServer)

import qualified Network as N
import Network.Socket (Socket)

import Utils (PrintChannel, createClient, displayKV
              , getAddress, getKeyStr, getSocket
              , startPrintChannel, syncPrint)

usage :: IO ()
usage = do
    n <- getProgName
    let estr = "Usage: " ++ n ++ " [--debug]\n\nSupplying a 1 means to " ++
               "display debugging messages.\n\n"
    hPutStrLn stderr estr

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> return ()
        ["--debug"] -> updateGlobalLogger "SAMP" (setLevel DEBUG)
        _ -> usage >> exitFailure

    withSAMP setupSnoop

setupSnoop :: IO ()
setupSnoop = do
    sock <- getSocket

    -- is this excessive?
    let closeSock = N.sClose sock

        hdlr :: String -> IO ()
        hdlr emsg = closeSock
                    >> hPutStrLn stderr ("ERROR: " ++ emsg)
                    >> exitFailure
         
        ioHdlr :: CE.IOException -> IO ()
        ioHdlr e = let emsg = if isUserError e
                              then ioeGetErrorString e
                              else show e
                   in hdlr emsg

        -- this assumes the only way to get a ThreadKilled is via a shutdown message
        asyncHdlr :: CE.AsyncException -> IO ()
        asyncHdlr e = let emsg = if e == CE.ThreadKilled
                                 then "SAMP Hub has shut down."
                                 else show e
                      in hdlr emsg

        otherHdlr :: CE.SomeException -> IO ()
        otherHdlr e = hdlr (show e)

        handlers = [ CE.Handler ioHdlr
                   , CE.Handler asyncHdlr
                   , CE.Handler otherHdlr ]

    snoop sock `CE.catches` handlers
    putStrLn "Closing the socket..."
    closeSock
    exitSuccess

sName :: RString
sName = "samp.name"

{-

We need to create a server to service messages from the Hub as well as
the SAMP client. We don't actually start the server up until after
registering for the calls, which means there is a small window when we
won't receive messages, but I am willing to live with this for now.

-}

snoop :: Socket -> IO ()
snoop sock = do
    -- create the client
    conn <- runE (createClient "hsamp-snooper"
                      "Report on messages sent by the hub.")

    -- the assumption is that we only need to unregister if we get
    -- a user interrupt but not for other errors; this doesn't seem
    -- that sensible an assumption, so remove for now
    {-
    let cleanUp :: CE.AsyncException -> IO ()
        cleanUp e = when (e == CE.UserInterrupt) (runE (unregisterE conn))
                    >> CE.throwIO e
    -}
    let cleanUp :: CE.SomeException -> IO ()
        cleanUp e = runE (unregisterE conn)
                    >> CE.throwIO e

    runSnooper sock conn `CE.catch` cleanUp

-- Set up the subscriptions and run the server to process the requests.

runSnooper :: Socket -> SAMPConnection -> IO ()
runSnooper sock conn = do
    -- what is the address of the server?
    url <- getAddress sock
    urlR <- runE (toRStringE url)
    putStrLn ("Snooping using " ++ url ++ "\n")

    -- register the messages that we are interested in
    runE (setXmlrpcCallbackE conn urlR >>
          declareSubscriptionsSimpleE conn ["*"])

    putStrLn "Registered messages..."

    -- now run the server; note that newClientMap queries the
    -- hub for the metadata of each client
    tid <- myThreadId
    (pchan, _) <- startPrintChannel
    clvar <- newClientMap conn

    putStrLn "Running the server..."
    runServer sock url (processCall conn tid pchan clvar)

{-

The client map is a mapping from the client Id (identifies the SAMP
client and is created by the hub) with a user friendly string, which
is a combination of the id and the samp.name setting of the client (if
defined).

-}

type ClientMap = M.Map ClientName String
type ClientMapVar = MVar ClientMap

bracket :: RString -> String
bracket a = " (" ++ fromRString a ++ ")"

toName :: ClientName -> Maybe RString -> String
toName k v = show k ++ maybe "" bracket v

-- TODO: can we use M.fromListWithKey here?

newClientMap :: SAMPConnection -> IO ClientMapVar
newClientMap conn = do
    clients <- runE (getClientNamesE conn)
    let conv (k,v) = (k, toName k v)
    newMVar (M.fromList (map conv clients))

getFromMap :: ClientMap -> ClientName -> String
getFromMap clmap n = M.findWithDefault (show n) n clmap

-- Get the display name for a client

getDisplayName :: ClientMapVar -> ClientName -> IO String
getDisplayName clvar clid = do
    clmap <- readMVar clvar
    return (getFromMap clmap clid)

-- Remove the client from the map, returning its display name

removeClient :: ClientMapVar -> ClientName -> IO String
removeClient clvar clid = 
    modifyMVar clvar $ \clmap -> do
        let rval = getFromMap clmap clid
        return (M.delete clid clmap, rval)

-- Add the client to the map, returning its display name. Will overwrite
-- existing values.

addClient :: ClientMapVar -> ClientName -> Maybe RString -> IO String
addClient clvar clid clname = do
    let name = toName clid clname
    modifyMVar clvar $ \clmap ->
        return (M.insert clid name clmap, name)

processCall ::
    SAMPConnection
    -> ThreadId
    -> PrintChannel
    -> ClientMapVar
    -> String
    -> IO ()
processCall conn tid pchan clvar = 
    simpleClientServer conn
       (notifications tid pchan clvar)
       (calls pchan clvar)
       (rfunc pchan clvar)

{-

TODO: should we have a handler for samp.hub.event.* which then 

  - checks for id field
  - processes remaining contents

or at least abstract out this for the messages we do support.

-}

notifications :: ThreadId -> PrintChannel -> ClientMapVar -> [SAMPNotificationFunc]
notifications tid pchan clvar =
     [("samp.hub.event.register", handleRegister pchan clvar),
      ("samp.hub.event.unregister", handleUnregister pchan clvar),
      ("samp.hub.event.metadata", handleMetadata pchan clvar),
      ("samp.hub.event.subscriptions", handleSubscriptions pchan clvar),
      ("samp.hub.event.shutdown", handleShutdown tid),
      ("*", handleOther pchan clvar)
     ]

calls :: PrintChannel -> ClientMapVar -> [SAMPCallFunc]
calls pchan clvar =
    [("samp.app.ping", handlePingCall pchan clvar),
     ("*", handleOtherCall pchan clvar)]

handleShutdown ::
    ThreadId
    -> MType
    -> ClientSecret
    -> ClientName
    -> [SAMPKeyValue]
    -> IO ()
handleShutdown tid _ _ _ _ = killThread tid

-- Return the value of the key from the input list, along with the
-- remaining key,value pairs.

getKeyVal ::
    ([SAMPKeyValue] -> RString -> Maybe a)
    -> [SAMPKeyValue]
    -> RString
    -> Maybe (a, [SAMPKeyValue])
getKeyVal get kvs key = 
    let f a = (a, filter ((/= key) . fst) kvs)
    in fmap f (get kvs key)

-- Look for id field in the params and remove it. for specific mtypes
-- we know the other required/suggested keys too.

maybeWithLabel :: 
  ([SAMPKeyValue] -> RString -> Maybe a) 
  -> RString 
  -> [SAMPKeyValue] 
  -> (a -> [SAMPKeyValue] -> IO ()) 
  -> IO () 
  -> IO ()
maybeWithLabel get lbl keys hasLbl noLbl  = 
   case getKeyVal get keys lbl of
       Nothing -> noLbl
       Just (clid,kvs) -> hasLbl clid kvs

-- Can the above be done something like fmap (uncurry hasLbl) ...

maybeWithId ::
    [SAMPKeyValue]
    -> (ClientName -> [SAMPKeyValue] -> IO ())
    -> IO ()
    -> IO ()
maybeWithId =
    let getIt kvs k = toClientName <$> getKeyStr kvs k
    in maybeWithLabel getIt "id"

-- Note: ensure we end with a blank line

displayWithKeys :: [String] -> [SAMPKeyValue] -> [String] -> [String]
displayWithKeys hdr keys footer = hdr ++ map displayKV keys ++ footer ++ [""]

withId ::
    String
    -> PrintChannel
    -> [SAMPKeyValue]
    -> (ClientName -> [SAMPKeyValue] -> IO ())
    -> IO ()
withId lbl pchan keys hasId = 
   let noId = syncPrint pchan $ displayWithKeys
                  ["Hub event: " ++ lbl] keys []
   in maybeWithId keys hasId noId

displaySecret :: ClientSecret -> String
displaySecret = ("  Secret    : " ++) . fromRString . fromClientSecret

displayMsgId :: MessageId -> String
displayMsgId  = ("  Message id: " ++) . fromRString . fromMessageId

handleRegister ::
    PrintChannel
    -> ClientMapVar
    -> MType
    -> ClientSecret
    -> ClientName
    -> [SAMPKeyValue] -> IO ()
handleRegister pchan clvar _ _ name keys = 
    withId "registration" pchan keys $ \clid kvs -> do
        clname <- addClient clvar clid Nothing
        syncPrint pchan $ displayWithKeys
            ["Client has added itself to " ++ show name ++ ": " ++ clname] kvs []

handleUnregister ::
    PrintChannel
    -> ClientMapVar
    -> MType
    -> ClientSecret
    -> ClientName
    -> [SAMPKeyValue]
    -> IO ()
handleUnregister pchan clvar _ _ name keys = 
    withId "unregistration" pchan keys $ \clid kvs -> do
        clname <- removeClient clvar clid
        syncPrint pchan $ displayWithKeys
            ["Client has removed itself from " ++ show name ++ ": " ++ clname] kvs []

handleMetadata ::
    PrintChannel
    -> ClientMapVar
    -> MType
    -> ClientSecret
    -> ClientName
    -> [SAMPKeyValue]
    -> IO ()
handleMetadata pchan clvar _ _ name keys = 
    withId "metadata" pchan keys $ \clid kvs -> do
        oclname <- getDisplayName clvar clid
        let doIt mdata k2 = 
              case mdata of
                  SAMPMap mds -> do
                                   nclname <- addClient clvar clid (getKeyStr mds sName)
                                   let clname = oclname ++ if oclname == nclname then "" else " -> " ++ nclname
                                   syncPrint pchan $ displayWithKeys
                                       ["Metadata notification from " ++
                                        show name ++ " for " ++ clname]
                                       mds (if null k2 then [] else " Other arguments:" : map displayKV k2)

                  _ -> syncPrint pchan $ displayWithKeys
                         ["Metadata notification from " ++ show name ++
                          " for " ++ oclname,
                         "  ERROR Expected metadata to be a map!"] kvs []

            failIt = syncPrint pchan $ displayWithKeys
                       ["Metadata notification from " ++ show name ++
                        " for " ++ oclname,
                       "  ERROR missing metadata parameter"] kvs []

        maybeWithLabel (flip lookup) "metadata" kvs doIt failIt

handleSubscriptions ::
    PrintChannel
    -> ClientMapVar
    -> MType
    -> ClientSecret
    -> ClientName
    -> [SAMPKeyValue]
    -> IO ()
handleSubscriptions pchan clvar _ _ name keys = 
    withId "subscriptions" pchan keys $ \clid kvs -> do
        clname <- getDisplayName clvar clid
        let doIt subs k2 = 
              case subs of
                  SAMPMap sds -> syncPrint pchan $ displayWithKeys
                                     ["Subscriptions notification from " ++
                                      show name ++ " for " ++ clname]
                                     sds (if null k2 then [] else " Other arguments:" : map displayKV k2)

                  _ -> syncPrint pchan $ displayWithKeys
                         ["Subscriptions notification from " ++ show name ++
                          " for " ++ clname,
                         "  ERROR Expected subscriptions to be a map!"] kvs []

            failIt = syncPrint pchan $ displayWithKeys
                       ["Subscriptions notification from " ++ show name ++
                        " for " ++ clname,
                       "  ERROR missing subscriptions parameter"] kvs []

        maybeWithLabel (flip lookup) "subscriptions" kvs doIt failIt

handleOther ::
    PrintChannel
    -> ClientMapVar
    -> MType
    -> ClientSecret
    -> ClientName
    -> [SAMPKeyValue]
    -> IO ()
handleOther pchan clvar mtype secret name keys = 
    let noId = do
            clname <- getDisplayName clvar name
            syncPrint pchan $ displayWithKeys
                ["Notification of " ++ show mtype ++ " from " ++ clname,
                 displaySecret secret] keys []
        
        hasId clid kvs = do
            clname <- getDisplayName clvar clid
            syncPrint pchan $ displayWithKeys
                ["Notification of " ++ show mtype ++ " from " ++
                 show name ++ " for " ++ clname,
                 displaySecret secret] kvs []

    in maybeWithId keys hasId noId

handlePingCall ::
    PrintChannel
    -> ClientMapVar
    -> MType
    -> ClientSecret
    -> ClientName
    -> MessageId
    -> [SAMPKeyValue]
    -> IO SAMPResponse
handlePingCall pchan clvar _ secret name msgid keys = do
    clname <- getDisplayName clvar name
    syncPrint pchan $ displayWithKeys
      ["hsamp-snooper was pinged by " ++ clname
      , displayMsgId msgid
      , displaySecret secret] keys []
    return (toSAMPResponse [] [])

-- Return a warning to point out that we are just logging this message
-- (basically copying the behavior of Mark's snooper here).

handleOtherCall ::
    PrintChannel
    -> ClientMapVar
    -> MType
    -> ClientSecret
    -> ClientName
    -> MessageId
    -> [SAMPKeyValue]
    -> IO SAMPResponse
handleOtherCall pchan clvar mtype secret name msgid keys = do
    clname <- getDisplayName clvar name
    syncPrint pchan $ displayWithKeys
      ["Call of " ++ show mtype ++ " by " ++ clname, displayMsgId msgid, displaySecret secret] keys []
    let emsg = fromJust (toRString ("The message " ++ show mtype ++
                                    " has only been logged, not acted on."))
    return (toSAMPResponseWarning [] emsg [] [])

-- TODO: handle warning case, although when is this ever called?

rfunc :: PrintChannel -> ClientMapVar -> SAMPResponseFunc
rfunc pchan clvar secret clid msgTag rsp = do
   clname <- getDisplayName clvar clid
   let msg = if isSAMPSuccess rsp
               then ["Got a response to msg-tag=" ++ show msgTag ++
                     " from=" ++ show clname,
                     displaySecret secret]
               else ["Error in response to msg-tag=" ++ show msgTag ++
                     " from=" ++ show clname,
                     show (fromJust (getSAMPResponseErrorTxt rsp))]
   syncPrint pchan msg

