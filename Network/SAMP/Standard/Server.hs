{-# LANGUAGE FlexibleInstances #-}

{-|
Module      :  Network.SAMP.Standard.Server
Copyright   :  (c) Smithsonian Astrophysical Observatory 2011
License     :  BSD-like

Maintainer  :  dburke@cfa.harvard.edu
Stability   :  unstable
Portability :  requires haxr

Implementation of the server side of the Standard
SAMP profile; that is, support for those clients that
are callable. Work on supporting a SAMP hub is planned.

Logging is provided using the @SAMP.StandardProfile.Server@
'System.Log.Logger.Logger' instance. At present this is limited
to debugging information only.
-}

module Network.SAMP.Standard.Server (

       -- * Asynchronous client calls
 
       callE,
       callAllE,
       setXmlrpcCallbackE,

       -- * High level server

       SAMPNotificationFunc, SAMPCallFunc, SAMPResponseFunc,
       simpleServer,

       -- * Low level server

       -- | Most clients should not need to use these routines.

       SAMPMethod, SAMPServerResult,
       SAMPFun, fun,

       SAMPMethodMap,
       methods, handleSAMPCall,

       ) where

import System.Log.Logger

import Network.XmlRpc.Internals

import Control.Monad.Trans (liftIO)
import qualified Control.Exception as CE

import Network.SAMP.Standard.Types
import Network.SAMP.Standard.Client (makeCallE, replyE)

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
--   @(SAMPType t1, ..., SAMPType tn) => 
--   t1 -> ... -> tn -> IO ()@
--   into a 'SAMPMethod'
fun :: SAMPFun a => a -> SAMPMethod
fun = toSAMPFun

class SAMPFun a where
    toSAMPFun :: a -> SAMPMethod

instance SAMPFun (IO ()) where
    toSAMPFun x (SAMPMethodCall _ []) = handleIO x >> return ()
    toSAMPFun _ _ = fail "Too many arguments"

instance (SAMPType a, SAMPFun b) => SAMPFun (a -> b) where
    toSAMPFun f (SAMPMethodCall n (x:xs)) = do
				  v <- fromSValue x
				  toSAMPFun (f v) (SAMPMethodCall n xs)
    toSAMPFun _ _ = fail "Too few arguments"

-- | Register a XML-RPC endpoint that the client uses to receive
-- information from the hub. This must be set up before either
-- 'callE' or 'callAllE' can be used.
setXmlrpcCallbackE :: SAMPConnection
                   -> RString -- ^ the URL of the end point
                   -> Err IO ()
setXmlrpcCallbackE cl url =
    makeCallE (sampHubURL cl) "samp.hub.setXmlrpcCallback"
        [SAMPString (sampPrivateKey cl), SAMPString url]
    >> return ()

-- | Send a message asynchronously to the recipient. The return value is
-- the message id given to this communication by the hub.
-- The client must be callable for this to work (see 'setXmlrpcCallbackE'),
-- although this module does not enforce this.
callE :: SAMPConnection
      -> RString -- ^ the name of the client to contact
      -> RString -- ^ a unique identifier for the communication (the message tag)
      -> SAMPMessage -- ^ the message
      -> Err IO RString -- ^ the message identifier created by the hub for this communication
callE cl clid msgtag msg =
    makeCallE (sampHubURL cl) "samp.hub.call"
        [SAMPString (sampPrivateKey cl), SAMPString clid, SAMPString msgtag, toSValue msg]
    >>= fromSValue

-- | Send a message asynchronously to all clients which are subscribed to the
-- message type.
-- The client must be callable for this to work (see 'setXmlrpcCallbackE'),
-- although this module does not enforce this.
callAllE :: SAMPConnection
         -> RString -- ^ a unique identifier for the communication (the message tag)
         -> SAMPMessage -- ^ the message
         -> Err IO [SAMPKeyValue] -- ^ the key is the name of the client and the value is the message id for that communication
callAllE cl msgtag msg =
    makeCallE (sampHubURL cl) "samp.hub.callAll"
        [SAMPString (sampPrivateKey cl), SAMPString msgtag, toSValue msg]
    >>= fromSValue
    
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

-- | A mapping from a 'MType' to the routine used to handle notification
-- of this message.
type SAMPNotificationFunc = (MType, RString -> RString -> [SAMPKeyValue] -> IO ())

-- | A mapping from a 'MType' to the routine used to handle calls
-- of this message. The response will be returned to the hub using the
-- @samp.hub.reply@ message (using 'Network.SAMP.Standard.Client.replyE').
type SAMPCallFunc = (MType, RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse)

-- | The handler for SAMP response messages (those received by a callable client
-- via the @samp.client.receiveResponse@ message).
type SAMPResponseFunc = RString -> RString -> RString -> SAMPResponse -> IO ()

receiveNotification :: [SAMPNotificationFunc] -> RString -> RString -> SAMPMessage -> IO ()
receiveNotification funcs secret senderid sm = do
    dbg "In receiveNotification"                    
    let mtype = getSAMPMessageType sm
        mparams = getSAMPMessageParams sm
    dbg $ "Notification mtype=" ++ show mtype ++ " sender=" ++ show senderid
    case lookup mtype funcs of
      Just func -> func secret senderid mparams
      _ -> do
             let emsg = "Unrecognized mtype for notification: " ++ show mtype
             dbg emsg
             fail emsg

receiveCall :: [SAMPCallFunc] -> SAMPConnection -> RString -> RString -> RString -> SAMPMessage -> IO ()
receiveCall funcs ci secret senderid msgid sm = do
    dbg "In receiveCall"
    let mtype = getSAMPMessageType sm
        mparams = getSAMPMessageParams sm
    dbg $ "Call mtype=" ++ show mtype ++ " sender=" ++ show senderid
    case lookup mtype funcs of
      Just func -> do
                     rsp <- func secret senderid msgid mparams
                     dbg $ "Responding with " ++ show rsp
                     handleError (const (return ())) $ replyE ci msgid rsp
      _ -> do
             let emsg = "Unrecognized mtype for call: " ++ show mtype
             dbg emsg
             fail emsg

receiveResponse :: SAMPResponseFunc -> RString -> RString -> RString -> SAMPResponse -> IO ()
receiveResponse f secret receiverid msgid rsp = do
    dbg $ "Received a response to message=" ++ show msgid ++ " receiver=" ++ show receiverid
    f secret receiverid msgid rsp

{-|
A map from SAMP method name to Haskell routine used to process
the message.
-}
type SAMPMethodMap = [(String, SAMPMethod)]

{-|
Look up the Haskell function to handle the SAMP method call
and execute it.

This routine includes logging to the @SAMP.StandardProfile.Server@
logger (at present only debug-level information).
-}
methods :: SAMPMethodMap -> SAMPMethodCall -> SAMPServerResult
methods xs c@(SAMPMethodCall name _) = do
    let mname = show name
    dbgE $ "Executing SAMP method: " ++ mname
    method <- maybeToM ("Unknown SAMP method: " ++ mname) (lookup mname xs)
    method c

{-
TODO: make this available to users so that they can append to it
if necessary.
-}
methodList :: SAMPConnection -> [SAMPNotificationFunc] -> [SAMPCallFunc] -> SAMPResponseFunc -> SAMPMethodMap
methodList ci ns cs r =
           [("samp.client.receiveNotification", fun (receiveNotification ns)),
           ("samp.client.receiveCall", fun (receiveCall cs ci)),
           ("samp.client.receiveResponse", fun (receiveResponse r))
           ]

{-|
Reads a SAMP method call from a string and uses the supplied method
to process the call.
-}
handleSAMPCall :: (SAMPMethodCall -> SAMPServerResult) -- ^ method to call
           -> String -- ^ XmlRpc input containing the SAMP details of the call
           -> IO ()
handleSAMPCall f str = do
    dbg $ "SAMP body of call is:\n" ++ str
    handleError (const (return ())) (parseSAMPCall str >>= f)

-- | A simple SAMP server for a single call.
simpleServer :: SAMPConnection -- ^ the connection information for the hub
             -> [SAMPNotificationFunc] -- ^ routines for handling notifications (@samp.client.receiveNotification@)
             -> [SAMPCallFunc] -- ^ routines for handling calls (@samp.client.receiveCall@)
             -> SAMPResponseFunc -- ^ routinr for handling responses (@samp.client.receiveResponse@)
             -> String -- ^ the Xml-RPC input containing the SAMP details of the call
             -> IO ()
simpleServer ci ns cs r = handleSAMPCall (methods (methodList ci ns cs r))
