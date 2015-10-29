{-# LANGUAGE OverloadedStrings #-}

{-|
Module      :  Network.SAMP.Standard.Client
Copyright   :  (c) Douglas Burke 2011, 2013, 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr

Client code for the Standard SAMP profile.

Note that this is for clients that do not need
to use the asynchronous call backs to receive
information from a hub. These routines are provided
by the "Network.SAMP.Standard.Server" module.

-}

module Network.SAMP.Standard.Client (

       -- * High-level interface
       registerClientE,
       unregisterE,
       toMetadata, toMetadataE,
       declareMetadataE,
       getMetadataE,
       getClientNameE, getClientNamesE,
       declareSubscriptionsE, declareSubscriptionsSimpleE,
       getSubscriptionsE,
       getRegisteredClientsE,
       getSubscribedClientsE,
       notifyE,
       notifyAllE, notifyAllE_,
       callAndWaitE,
       replyE,
       pingE,
       pingHubE,

       -- * Low-level interface

       makeCallE,
       makeCallE_,
       callHubE,
       callHubE_,
       registerE, getClientInfoE

       ) where

import qualified Control.Arrow as CA
import qualified Control.Exception as CE

import qualified Data.Traversable as T

import Control.Monad (ap, forM, liftM, forM, void)
import Control.Monad.Except (throwError, runExceptT)
import Control.Monad.Trans (liftIO)

import Data.Maybe (fromJust, fromMaybe, catMaybes)

import Network.SAMP.Standard.Types
import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import System.Log.Logger


-- the name of the SAMP client logging instance
cLogger :: String
cLogger = "SAMP.StandardProfile.Client"

-- log the message to the SAMP client logger at the debug level.
dbg :: String -> Err IO ()
dbg = liftIO . debugM cLogger

{-|
Call the hub with the given message and arguments.
This is a low-level routine and users are expected to
use more appropriate routines where available.

This routine includes logging to the @SAMP.StandardProfile.Client@
logger (at present only debug-level information).
-} 

makeCallE :: 
  String              -- ^ url of the hub
  -> RString          -- ^ message name
  -> [SAMPValue]      -- ^ message arguments
  -> Err IO SAMPValue -- ^ response
makeCallE url msg args = do
  let rmsg = fromRString msg
      
      -- try and produce a somewhat useful error message for when
      -- the hub can't be contacted.
      --
      hdlr :: CE.IOException -> Err IO a
      hdlr e =
        let s = show e
        in throwError $ if s == "connect: does not exist (Connection refused)"
                        then "Is a SAMP hub running? Unable to connect to " ++ url
                        else s

  dbg $ "Calling message " ++ rmsg ++ " at " ++ url
  dbg $ "  with args " ++ show args
  rsp <- call url rmsg (map toValue args) `catchErrT` hdlr
  dbg ("Response to " ++ rmsg)
  dbg (show rsp)
  fromValue rsp

{-  CURRENTLY unused

-- | Raise an error if the SAMPValue does not represent a
--   successful (or partially-successful) response.
--
--   TODO: should this be part of the standard sequence,
--         and should SAMPResponse be used instead of SAMPValue?

validateSAMPValue :: SAMPValue -> Err IO SAMPValue
validateSAMPValue sv = do
  dbg ("Validating: " ++ show sv)
  sr <- fromSValue sv
  case getSAMPResponseError sr of
    Nothing -> return sv
    Just (msg, _) -> throwError ("SAMP call failed: " ++ fromRString msg)

-}

-- | This is the catchErrorT routine from https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/exceptions-and-monad-transformers
--   specialized to the Err IO monad (which is ErrorT String IO).
catchErrT ::
  (CE.Exception e)
  => Err IO a
  -> (e -> Err IO a)
  -> Err IO a
catchErrT a onE = do
  eresult <- liftIO $ runExceptT a `CE.catch` (runExceptT . onE)
  either throwError return eresult

-- | Like 'makeCallE' but ignores the return value.
makeCallE_ :: 
  String          -- ^ url of the hub
  -> RString      -- ^ message name
  -> [SAMPValue]  -- ^ message arguments
  -> Err IO ()    -- ^ response
makeCallE_ url msg args = void (makeCallE url msg args)

{-|
Similar to 'makeCallE' but takes a 'SAMPConnection' 
as an argument. It automatically adds the private
key to the start of the message arguments.

This is a low-level routine and users are expected to
use more appropriate routines where available.
-} 
callHubE :: SAMPConnection
         -> RString      -- ^ message name
         -> [SAMPValue]  -- ^ message arguments
         -> Err IO SAMPValue     -- ^ response
callHubE conn msg args = 
         makeCallE (scHubURL conn) msg
                   (toSValue (scPrivateKey conn) : args)

-- | As `callHubE` but ignores the return value.
callHubE_ :: 
  SAMPConnection
  -> RString      -- ^ message name
  -> [SAMPValue]  -- ^ message arguments
  -> Err IO ()    -- ^ response
callHubE_ conn msg args = void (callHubE conn msg args)

-- | Register a client with a hub. See 'registerClientE' for a simple
--   way to register the client and process the return vaues.
registerE ::
    SAMPInfo     -- ^ hub information
    -> Err IO [SAMPKeyValue]
       -- ^ Key/value pairs from the registration call.
registerE si =
  let sKey = fromHubSecret (getSAMPInfoHubSecret si)
      url = getSAMPInfoHubURL si
  in makeCallE url "samp.hub.register" [SAMPString sKey]
     >>= fromSValue
    
sPrivateKey , sHubId , sSelfId :: RString
sPrivateKey = "samp.private-key"
sHubId      = "samp.hub-id"
sSelfId     = "samp.self-id"

-- lookup a RString value from a SAMPKeyValue list;
-- could use fromSValue to parse the response but
-- this should give a slightly-more-useful error
-- message (containing both key and value)
--
slookup :: (Monad m) => RString -> [SAMPKeyValue] -> Err m RString
slookup k a =
    let conv :: Monad m => SAMPValue -> Err m RString
        conv (SAMPString s) = return s
        conv x = throwError ("Expected a string for key=" ++ show k ++
                             " but found " ++ show x)
        noKey = throwError ("Unable to find key " ++ show k)
    in maybe noKey conv (lookup k a)
                                       
-- | Create a 'SAMPConnection' record from the hub information and response
-- from 'registerE'.
getClientInfoE ::
    (Monad m)
    => SAMPInfo        -- ^ hub information
    -> [SAMPKeyValue]  -- ^ response from 'registerE'
    -> Err m SAMPConnection
getClientInfoE si ks =
  let sKey = getSAMPInfoHubSecret si
      url = getSAMPInfoHubURL si
  in SAMPConnection
         `liftM` return sKey 
         `ap` return url
         `ap` (toClientSecret <$> slookup sPrivateKey ks)
         `ap` slookup sHubId ks
         `ap` (toClientName <$> slookup sSelfId ks)

-- | Register the client with the hub and create the 'SAMPConnection' record
-- used for communication. This is basically
--
-- @
--    'registerE' si >>= 'getClientInfoE' si
-- @
registerClientE :: SAMPInfo -> Err IO SAMPConnection
registerClientE si = registerE si >>= getClientInfoE si

-- | Unregister the client from the hub.
unregisterE ::
    SAMPConnection
    -> Err IO ()
unregisterE cl =
    callHubE_ cl "samp.hub.unregister" []

sName , sTxt, sHtml, sIcon, sDoc :: RString
sName = "samp.name"
sTxt  = "samp.description.text"
sHtml = "samp.description.html"
sIcon = "samp.icon.url"
sDoc  = "samp.documentaion.url"

-- | Create the key/value pairs used by 'declareMetadataE'
-- for the common metadata settings. Also see 'toMetadataE'.
toMetadata :: RString -- ^ A one-word title for the application (@samp.name@)
           -> Maybe RString -- ^ A short description of the application in plain text (@samp.description.text@)
           -> Maybe RString -- ^ A description of the application in HTML (@samp.description.html@)
           -> Maybe RString -- ^ The URL of an icon in png, gif or jpeg format (@samp.icon.url@)
           -> Maybe RString -- ^ The URL of a documentation web page (@samp.documentation.url@)
           -> [SAMPKeyValue]
toMetadata m txt html icon doc =
    let f k = fmap $ (,) k . SAMPString
        ms = [f sTxt txt, f sHtml html, f sIcon icon, f sDoc doc]
    in (sName, SAMPString m) : catMaybes ms

-- | Create the key/value pairs used by 'declareMetadataE'
-- for the common metadata settings. Also see 'toMetadata'.
toMetadataE ::
    Monad m
    => String -- ^ A one-word title for the application (@samp.name@)
    -> Maybe String
       -- ^ A short description of the application in plain text
       --   (@samp.description.text@)
    -> Maybe String
       -- ^ A description of the application in HTML (@samp.description.html@)
    -> Maybe String
       -- ^ The URL of an icon in png, gif or jpeg format (@samp.icon.url@)
    -> Maybe String
       -- ^ The URL of a documentation web page (@samp.documentation.url@)
    -> Err m [SAMPKeyValue]
toMetadataE m txt html icon doc = 
    let f k (Just v) = toRStringE v >>= \vs -> return $ Just (k, SAMPString vs)
        f _ _ = return Nothing
        ms = [f sName (Just m), f sTxt txt, f sHtml html, f sIcon icon, f sDoc doc]
    in liftM catMaybes $ sequence ms

{-|
Declare the metadata for the client. The metadata is provided as a
list of (key,value) pairs (this is slightly different from the SAMP
API which has the return being a map; here we extract the contents of
the map). See 'toMetadata' and 'toMetadataE' for convenience routines
for setting the common metadata fields.

This overwrites the existing metadata stored in the hub for the
client.
-}
declareMetadataE ::
    SAMPConnection
    -> [SAMPKeyValue] -- ^ the key/value pairs to declare 
    -> Err IO ()
declareMetadataE cl ks = 
    callHubE_ cl "samp.hub.declareMetadata" [SAMPMap ks]

-- | Return the metadata for another client of the SAMP hub as a
-- list of (key,value) pairs.
getMetadataE ::
    SAMPConnection
    -> ClientName -- ^ The id of the SAMP client to query
    -> Err IO [SAMPKeyValue]
       -- ^ The metadata key/value pairs of the queried client
getMetadataE conn clid =
    callHubE conn "samp.hub.getMetadata" [toSValue clid]
    >>= fromSValue

mtToRS :: MType -> RString
mtToRS = fromJust . toRString . fromMType

-- | Declare the subscriptions for this client. The subscriptions are
-- given as a list of (key,value) pairs where the keys are the MTypes
-- of the messages to subscribe to and the value is determined by the
-- selected 'MType'.
-- 
-- This overwrites the existing subscriptions for the client.
-- 
-- See 'declareSubscriptionsSimpleE' for the case when the messages
-- being subscribed to have no parameters.
declareSubscriptionsE ::
    SAMPConnection
    -> [(MType, SAMPValue)]
    -- ^ the messages (and associated metadata) the client is subscribing to
    -> Err IO ()
declareSubscriptionsE cl subs =
    let ks = map (CA.first mtToRS) subs
    in callHubE_ cl "samp.hub.declareSubscriptions" [SAMPMap ks]

-- | Declare the subscriptions for this client. This can be used
-- if all the messages require no parameters; use 
-- 'declareSubscriptionsE' for the general case.
declareSubscriptionsSimpleE ::
    SAMPConnection
    -> [MType] -- ^ the messages the client is subscribing to
    -> Err IO ()
declareSubscriptionsSimpleE cl mtypes =
    let ks = map (\n -> (mtToRS n, SAMPMap [])) mtypes
    in callHubE_ cl "samp.hub.declareSubscriptions" [SAMPMap ks]

-- | Get the message subscriptions of a client. The subscriptions are
-- returned as a list of (key,value) pairs.
getSubscriptionsE ::
    SAMPConnection
    -> ClientName -- ^ the name of the client to query
    -> Err IO [SAMPKeyValue]
       -- ^ the (key,value) subscriptions of the queried client
getSubscriptionsE cl clid = 
    callHubE cl "samp.hub.getSubscriptions" [toSValue clid]
    >>= fromSValue

-- | Return a list of all the registered clients of the hub - including
-- itself - but excluding this client.
getRegisteredClientsE ::
    SAMPConnection
    -> Err IO [ClientName] -- ^ the names of the registered clients
getRegisteredClientsE cl =
    callHubE cl "samp.hub.getRegisteredClients" []
    >>= fromSValue

-- | Return a (key,value) list of all the clients that are subscibed to
-- a given 'MType' (the return value is slightly different from the
-- SAMP API which has the return being a map; here we extract the 
-- contents of the map).
-- 
-- An error occurs if the MType contains a wild card.
getSubscribedClientsE ::
    SAMPConnection
    -> MType -- ^ the message (it can not contain a wildcard)
    -> Err IO [(ClientName, SAMPValue)]
getSubscribedClientsE cl mtype 
    | isMTWildCard mtype = throwError "MType can not contain a wild card when calling getSubscribedClients"
    | otherwise          =
        let conv :: SAMPKeyValue -> (ClientName, SAMPValue)
            conv = CA.first toClientName

            act = callHubE cl "samp.hub.getSubscribedClients" [toSValue mtype]
        in map conv `liftM` (act >>= fromSValue)

-- | Send a message to a given client of the hub and do not
-- wait for a response.
notifyE ::
    SAMPConnection
    -> ClientName -- ^ the name of the client to notify
    -> SAMPMessage -- ^ the message
    -> Err IO ()
notifyE cl clid msg =
    callHubE_ cl "samp.hub.notify" [toSValue clid, toSValue msg]

-- | Send a message to all clients and get back a list of those
-- that were sent the message (i.e. are subscribed to it). Note that
-- just because a client was sent a message does not mean it was successful.
notifyAllE :: 
  SAMPConnection
  -> SAMPMessage         -- ^ the message
  -> Err IO [ClientName] -- ^ the list of clients that were sent the message
notifyAllE cl msg =
    callHubE cl "samp.hub.notifyAll" [toSValue msg]
    >>= fromSValue

-- | 'notifyAllE' ignoring the return value.
notifyAllE_ :: 
  SAMPConnection
  -> SAMPMessage  -- ^ the message
  -> Err IO ()    
notifyAllE_ cl msg = void $ notifyAllE cl msg

-- | Send a message to a client and wait for a response. The timeout parameter
-- controls how long the wait will be before error-ing out (if given).
-- Unlike 'callE' and 'callAllE', the client need not be callable to use
-- this routine.
callAndWaitE ::
    SAMPConnection
    -> ClientName -- ^ the name of the client to contact
    -> SAMPMessage -- ^ the message
    -> Maybe Int -- ^ the maximum timeout to wait, in seconds
    -> Err IO SAMPResponse
callAndWaitE cl clid msg tout = 
    let t = fromMaybe 0 tout
        args = [toSValue clid, toSValue msg, toSValue t]
    in callHubE cl "samp.hub.callAndWait" args >>= fromSValue
    
-- | Reply to a message from another client.
replyE ::
    SAMPConnection
    -> MessageId -- ^ the message identifier (as returned by the hub from 'callE' or 'callAllE')
    -> SAMPResponse -- ^ the response
    -> Err IO ()
replyE cl msgid rsp =
    callHubE_ cl "samp.hub.reply" [toSValue msgid, toSValue rsp]

{-|
Ping the hub to see if it is running.

Note that we do not support calling this method without
the private key (taken from the 'SAMPConnection' record).
Users who wish to see if a hub is alive and just have a URL
can try 'pingHubE' or using 'makeCallE_' directly - e.g.

>    makeCallE_ url "samp.hub.ping" []
-}
pingE :: 
  SAMPConnection
  -> Err IO ()
pingE cl = callHubE_ cl "samp.hub.ping" []

-- | Ping a hub to see if it is responding to queries. Unlike
--   'pingE' this takes the Hub URL; i.e. it does not requires
--   that a connection to the hub has already been made.
--
pingHubE ::
    String        -- ^ URL of the hub
    -> Err IO ()
pingHubE url = makeCallE_ url "samp.hub.ping" []
           
{-|
Get the names (@samp.name@) of all the registered clients (excluding
this one).
-}
getClientNamesE :: 
  SAMPConnection
  -> Err IO [(ClientName, Maybe RString)]
     -- ^ key is the client id and the value is the @samp.name@ value (if set)
getClientNamesE conn = do
    clients <- getRegisteredClientsE conn
    forM clients $ \clid -> (,) clid <$> getClientNameE conn clid

{-|
Get the name (@samp.name@) of the client, if set.
-}
getClientNameE :: 
  SAMPConnection
  -> ClientName -- ^ the client id
  -> Err IO (Maybe RString) -- ^ the @samp.name@ value for the client, if set
getClientNameE conn clid = do
  md <- getMetadataE conn clid
  T.sequence (fromSValue <$> lookup sName md)

