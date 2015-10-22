{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      :  Network.SAMP.Standard.Server
Copyright   :  (c) Douglas Burke 2011, 2013, 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr

Implementation of the server side of the Standard
SAMP profile; that is, support for those clients that
are callable. Work on supporting a SAMP hub is planned.

Logging is provided using the @SAMP.StandardProfile.Server@
'System.Log.Logger.Logger' instance. At present this is limited
to debugging information only.

TODO:

  - look at adding a type synonym for message id\/tag\/secret
    arguments to make it clearer what is expected
-}

{-
TODO: look at the hbeanstalk code on hackage as may be relevant;
also other systems like ZeroMQ/RabbitMQ.
-}

module Network.SAMP.Standard.Server (

       -- * Asynchronous client calls
 
       callE,
       callAllE,
       setXmlrpcCallbackE,

       -- * High level server

       -- | These routines are used to define how to handle the
       -- SAMP messages from the hub. The match is supplied by the
       -- 'MType' components of the  'SAMPNotificationFunc' and
       -- 'SAMPCallFunc' types. These can include wildtypes
       -- if you want a single handler for multiple messages.
       -- This /may/ change to supplying a comparator (e.g.
       -- @MType -> Bool@) if it is found to be necessary.
       --

       SAMPNotificationFunc, SAMPCallFunc, SAMPResponseFunc,
       simpleClientServer,

       -- * Low level server

       -- | Most clients should not need to use these routines.

       SAMPMethod, SAMPServerResult,
       SAMPFun, fun,

       SAMPMethodMap,
       clientMethods, handleSAMPCall,
       clientMethodMap

       ) where

import System.Log.Logger

import Network.XmlRpc.Internals

import Control.Monad.Trans (liftIO)
import qualified Control.Exception as CE

import Network.SAMP.Standard.Types
import Network.SAMP.Standard.Client (callHubE, callHubE_, replyE)

import Control.Monad (void)

-- the name of the SAMP client logging instance
sLogger :: String
sLogger = "SAMP.StandardProfile.Server"

-- log the message to the SAMP server logger at the debug level.
dbgE :: String -> Err IO ()
dbgE = liftIO . debugM sLogger

dbg :: String -> IO ()
dbg = debugM sLogger

showException :: CE.SomeException -> String
showException = show

handleIO :: IO a -> Err IO a
handleIO io = liftIO (CE.try io) >>= either (fail . showException) return

-- | The return result
type SAMPServerResult = Err IO ()

-- | The type of SAMP methods on the server.
type SAMPMethod = (SAMPMethodCall -> SAMPServerResult)

--
-- Converting Haskell functions to SAMP methods.
-- This is very-heavily inspired by the haxr Server module
--

-- | Turns any function 
--   @(SAMPType t1, ..., SAMPType tn) => t1 -> ... -> tn -> IO ()@
--   into a 'SAMPMethod'
fun :: SAMPFun a => a -> SAMPMethod
fun = toSAMPFun

class SAMPFun a where
    toSAMPFun :: a -> SAMPMethod

instance SAMPFun (IO ()) where
    toSAMPFun x (SAMPMethodCall _ []) = void $ handleIO x
    toSAMPFun _ _ = fail "Too many arguments"

instance (SAMPType a, SAMPFun b) => SAMPFun (a -> b) where
    toSAMPFun f (SAMPMethodCall n (x:xs)) = do
                                  v <- fromSValue x
                                  toSAMPFun (f v) (SAMPMethodCall n xs)
    toSAMPFun _ _ = fail "Too few arguments"

-- | Register a XML-RPC endpoint that the client uses to receive
-- information from the hub. This must be set up before either
-- 'callE' or 'callAllE' can be used.
setXmlrpcCallbackE ::
    SAMPConnection
    -> RString -- ^ the URL of the end point
    -> Err IO ()
setXmlrpcCallbackE conn url =
    callHubE_ conn "samp.hub.setXmlrpcCallback" [SAMPString url]

-- | Send a message asynchronously to the recipient. The return value is
-- the message id given to this communication by the hub.
-- The client must be callable for this to work (see 'setXmlrpcCallbackE'),
-- although this module does not enforce this.
callE ::
    SAMPConnection
    -> ClientName -- ^ the name of the client to contact
    -> MessageTag -- ^ a unique identifier for the communication
    -> SAMPMessage -- ^ the message
    -> Err IO MessageId -- ^ the message identifier created by the hub for this communication
callE conn clid msgtag msg =
    callHubE conn "samp.hub.call"
        [toSValue clid, toSValue msgtag, toSValue msg]
    >>= fromSValue

-- | Send a message asynchronously to all clients which are subscribed to the
-- message type.
--
callAllE ::
    SAMPConnection
    -> MessageTag -- ^ a unique identifier for the communication
    -> SAMPMessage -- ^ the message
    -> Err IO [(ClientName, MessageId)]
callAllE conn msgtag msg = do
    let conv (k,v) = do
          mid <- fromSValue v
          return (toClientName k, mid)
    rsp <- callHubE conn "samp.hub.callAll" [toSValue msgtag, toSValue msg]
    kvals <- fromSValue rsp
    mapM conv kvals
    
{-
From SAMP 1.2 document, callable clients must support

nb a hidden first argument of string private-key

receiveNotification(string sender-id, map message)

Method called by the hub when dispatching a notification to its recip-
ient. The form of the message map is given in Section 3.8.

receiveCall(string sender-id, string msg-id, map message)

Method called by the hub when dispatching a call to its recipient. The
client MUST at some later time make a matching call to reply() on the
hub. The form of the message map is given in Section 3.8.

receiveResponse(string responder-id, string msg-tag, map response)

Method used by the hub to dispatch to the sender the response of an
earlier asynchronous call. The form of the response map is given in
Section 3.9.

-}

-- TODO: add in MessageId types as appropriate here

-- | A mapping from a 'MType' to the routine used to handle notification
-- of the @samp.client.receiveNotification@ message.
type SAMPNotificationFunc = (MType, MType -> ClientSecret -> ClientName -> [SAMPKeyValue] -> IO ())

-- | A mapping from a 'MType' to the routine used to handle calls
-- of the message @samp.client.receiveCall@. The response will be returned to the hub using the
-- @samp.hub.reply@ message (using 'Network.SAMP.Standard.Client.replyE')
-- when using 'simpleClientServer'.
type SAMPCallFunc =
    (MType,
     MType -> ClientSecret -> ClientName -> MessageId -> [SAMPKeyValue] -> IO SAMPResponse)

-- | The handler for SAMP response messages (those received by a callable client
-- via the @samp.client.receiveResponse@ message).
type SAMPResponseFunc =
    ClientSecret -> ClientName -> MessageTag -> SAMPResponse -> IO ()

receiveNotification ::
    [SAMPNotificationFunc]
    -> ClientSecret
    -> ClientName
    -> SAMPMessage
    -> IO ()
receiveNotification funcs secret senderid sm = do
    dbg "In receiveNotification"                    
    let mtype = getSAMPMessageType sm
        mparams = getSAMPMessageParams sm
    dbg $ "Notification mtype=" ++ show mtype ++ " sender=" ++ show senderid
    case lookup mtype funcs of
      Just func -> func mtype secret senderid mparams
      _ -> do
             let emsg = "Unrecognized mtype for notification: " ++ show mtype
             dbg emsg
             fail emsg

receiveCall ::
    [SAMPCallFunc]
    -> SAMPConnection
    -> ClientSecret
    -> ClientName
    -> MessageId
    -> SAMPMessage
    -> IO ()
receiveCall funcs ci secret senderid msgid sm = do
    dbg "In receiveCall"
    let mtype = getSAMPMessageType sm
        mparams = getSAMPMessageParams sm
    dbg $ "Call mtype=" ++ show mtype ++ " sender=" ++ show senderid
    case lookup mtype funcs of
      Just func -> do
                     rsp <- func mtype secret senderid msgid mparams
                     dbg $ "Responding with " ++ show rsp
                     rE $ replyE ci msgid rsp
      _ -> do
             let emsg = "Unrecognized mtype for call: " ++ show mtype
             dbg emsg
             fail emsg

receiveResponse ::
    SAMPResponseFunc
    -> ClientSecret
    -> ClientName
    -> MessageTag
    -> SAMPResponse
    -> IO ()
receiveResponse f secret receiverid msgTag rsp = do
    dbg ("Received a response to tag=" ++ show msgTag ++
         " receiver=" ++ show receiverid)
    f secret receiverid msgTag rsp

{-|
A map from SAMP method name to Haskell routine used to process
the message.
-}
type SAMPMethodMap = [(RString -> Bool, SAMPMethod)]

-- find the relevant entry in the input "map"

find :: a -> [(a -> Bool, b)] -> Maybe b
find _ [] = Nothing
find a ((f,g):xs) | f a       = Just g
                  | otherwise = find a xs

{-|
Look up the Haskell function to handle the SAMP method call
and execute it.

This routine includes logging to the @SAMP.StandardProfile.Server@
logger (at present only debug-level information).
-}
clientMethods :: SAMPMethodMap -> SAMPMethodCall -> SAMPServerResult
clientMethods xs c@(SAMPMethodCall name _) = do
    let mname = fromRString name
    dbgE $ "Executing SAMP method: " ++ mname
    method <- maybeToM ("Unknown SAMP method: " ++ mname) (find name xs)
    method c

{-| Returns a map of handlers for messages received by a
callable SAMP client. This can be appended to to handle
extra messages beyond the SAMP Standard Profile.
-}
clientMethodMap ::
    SAMPConnection -- ^ the connection to use when replying to a message
    -> [SAMPNotificationFunc] -- ^ routines for handling notifications (@samp.client.receiveNotification@)
    -> [SAMPCallFunc] -- ^ routines for handling calls (@samp.client.receiveCall@)
    -> SAMPResponseFunc -- ^ routinr for handling responses (@samp.client.receiveResponse@)
    -> SAMPMethodMap -- ^ input for the 'clientMethods' routine
clientMethodMap ci ns cs r =
    [((== "samp.client.receiveNotification"), fun (receiveNotification ns)),
     ((== "samp.client.receiveCall"), fun (receiveCall cs ci)),
     ((== "samp.client.receiveResponse"), fun (receiveResponse r))
    ]

-- run an action that returns nothing
rE :: (Monad m) => Err m () -> m ()
rE = handleError (const (return ())) 

{-|
Reads a SAMP method call from a string and uses the supplied method
to process the call.

This routine includes logging to the @SAMP.StandardProfile.Server@
logger (at present only debug-level information).
-}
handleSAMPCall ::
    (SAMPMethodCall -> SAMPServerResult) -- ^ method to call
    -> String -- ^ XML-RPC input containing the SAMP details of the call
    -> IO ()
handleSAMPCall f str = do
    dbg $ "SAMP body of call is:\n" ++ str
    rE $ parseSAMPCall str >>= f

-- | Handle the SAMP messages sent to a callable client. This processes a
-- single call using the supplied handlers.
simpleClientServer ::
    SAMPConnection -- ^ the connection information for the hub
    -> [SAMPNotificationFunc] -- ^ routines for handling notifications (@samp.client.receiveNotification@)
    -> [SAMPCallFunc] -- ^ routines for handling calls (@samp.client.receiveCall@)
    -> SAMPResponseFunc -- ^ routinr for handling responses (@samp.client.receiveResponse@)
    -> String -- ^ the Xml-RPC input containing the SAMP details of the call
    -> IO ()
simpleClientServer ci ns cs r =
    handleSAMPCall (clientMethods (clientMethodMap ci ns cs r))
