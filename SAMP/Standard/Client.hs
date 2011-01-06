{-

Client code for the Standard SAMP profile.
-}

module SAMP.Standard.Client (
       -- * High-level interface
       getHubInfoE,
       registerClientE,
       unregisterE,
       toMetadata, declareMetadataE,
       getMetadataE,
       declareSubscriptionsE, declareSubscriptionsSimpleE,
       getSubscriptionsE,
       getRegisteredClientsE,
       getSubscribedClientsE,
       notifyE,
       notifyAllE,
       callE,
       callAllE,
       callAndWaitE,
       replyE,
       pingE,
       setXmlrpcCallbackE,

       -- * Low-level interface

       makeCallE,
       registerE, getClientInfoE

       ) where

import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import System.Log.Logger

import qualified System.IO.Strict as S

import Data.List (stripPrefix)
import Data.Maybe (fromJust, fromMaybe, catMaybes)

import qualified Control.Arrow as CA
import Control.Monad (liftM, ap, guard, when)

import Control.Monad.Error (throwError)
import Control.Monad.Trans (liftIO)

import qualified Control.Exception as CE

import System.Environment (getEnv)
-- import System.IO.Error (catch)

import System.IO.Error (isDoesNotExistError, isDoesNotExistErrorType, ioeGetErrorType)

import SAMP.Standard.Types

-- the name of the SAMP client logging instance
cLogger :: String
cLogger = "SAMP.StandardProfile.Client"

-- log the message to the SAMP client logger at the debug level.
dbg :: String -> Err IO ()
dbg = liftIO . debugM cLogger

{-
XXX todo: need to update to SAMP 1.2
    (look for SAMP_HUB env file first)
    and Windows (USERPROFILE env var)

-}

getEnvM :: String -> IO (Either () String)
getEnvM key = CE.tryJust
        (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
        (getEnv key)

{-
TODO: improve error messages

It's also not 100% clear if falling through to the HOME/USERPROFILE
variable when the SAMP_HUB profile does not start with the std-lockurl
prefix is valid.
-}

stdPrefix :: String
stdPrefix = "std-lockurl:"

sampLockFile :: IO FilePath
sampLockFile = do
    e1 <- getEnvM "SAMP_HUB"
    case e1 of
         Right fname -> doHub fname
         Left _ -> doHome

  where
    doHome = do
        home <- getEnv "HOME" -- to do: use USERPROFILE on Windows
        return $ home ++ "/.samp"

    doHub fname = 
          case stripPrefix stdPrefix fname of
            Nothing -> doHome
            Just f -> do
                 putStrLn $ "I am supposed to do something with " ++ f
                 fail "lazy-programmer-error"

checkExists :: String -> IO a -> Err IO a
checkExists emsg act = do
   e <- liftIO $ CE.tryJust (guard . isDoesNotExistError) act
   case e of
     Right a -> return a
     Left _ -> throwError emsg

-- TODO: deal with SAMP_HUB environment variable
sampLockFileE :: Err IO FilePath
sampLockFileE = do
    home <- checkExists "The HOME environment variable is not defined." (getEnv "HOME")
    return $ home ++ "/.samp"

-- As the SAMP control file may well be written using Windows
-- control characters we need to ensure these are handled,
-- since lines just splits on \n. This routine is taken straight
-- from Real World Haskell.
--
splitLines :: String -> [String]
splitLines [] = []
splitLines cs = 
   let (pre, suf) = break isLineTerminator cs
       in pre : case suf of
           ('\r':'\n':rest) -> splitLines rest
           ('\r':rest) -> splitLines rest
           ('\n':rest) -> splitLines rest
           _           -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c `elem` "\r\n"

-- Remove un-needed lines from the SAMP lock file
-- and return a list of interesting lines
--
stripLines ::String -> [String]
stripLines = filter (\x ->not (null x || head x == '#')) . splitLines

-- Given the contents of a SAMP lockfile, return
-- a map containing all the assignments in the file.
-- We just use an association list for the return value
-- since the expected number of keys is small. The file
-- is assumed to be correctly formatted.
--
processLockFile :: String -> [(String,String)]
processLockFile = foldr step [] . stripLines
    where
        step = (:) . CA.second tail . break (== '=')
        -- step l a = CA.second tail (break (== '=') l) : a

extractHubInfo :: [(String,String)] -> Maybe SAMPInfo
extractHubInfo alist = do
    secret <- lookup "samp.secret" alist >>= toRString
    url <- lookup "samp.hub.xmlrpc.url" alist
    return (secret, url)

{-|
Return information about the SAMP hub.
It assumes we are on a UNIX environment and at present
ignores the SAMP_HUB environment variable.

This routine includes logging to the "SAMP.StandardProfile.Client"
logger (at present only debug-level information).
-}
getHubInfoE :: Err IO SAMPInfo
getHubInfoE = do
    fname <- sampLockFileE
    dbg $ "Looking for a SAMP lock file called: " ++ fname
    let emsg = "Unable to find Hub info (" ++ fname ++ ")"
    cts <- checkExists emsg $ S.readFile fname
    dbg "Lock file exists."
    let alist = processLockFile cts
    maybeToM "Unable to read hub info" (extractHubInfo alist)

{-
-- Checks that the response was okay, throwing an error if not.
-- Warnings are not considered to be an error here.

isOK :: SAMPResponse -> Err IO ()
isOK rsp | isSAMPErrorOnly rsp = throwError $ "ERROR from response:\n" ++ show (fromJust $ getSAMPResponseErrorTxt rsp)
         | otherwise           = return ()
-}

{-

From the SAMP documentation:

   # Read information from lockfile to locate and register with hub.
   string hub-url = readFromLockfile("samp.hub.xmlprc.url");
   string samp-secret = readFromLockfile("samp.secret");

   # Establish XML-RPC connection with hub
   # (uses some generic XML-RPC library)
   xmlrpcServer hub = xmlrpcConnect(hub-url);

   # Register with hub.
   map reg-info = hub.xmlrpcCall("samp.hub.register", samp-secret);
   string private-key = reg-info.getValue("samp.private-key");

   # Store metadata in hub for use by other applications.
   map metadata = ("samp.name" -> "dummy",
                   "samp.description.text" -> "Test Application",
                   "dummy.version" -> "0.1-3");
   hub.xmlrpcCall("samp.hub.declareMetadata", private-key, metadata);

   # Send a message requesting file load to all other 
   # registered clients, not wanting any response.
   map loadParams = ("filename" -> "/tmp/foo.bar");
   map loadMsg = ("samp.mtype" -> "file.load",
                  "samp.params" -> loadParams);
   hub.xmlrpcCall("samp.hub.notifyAll", private-key, loadMsg);

   # Unregister
   hub.xmlrpcCall("samp.hub.unregister", private-key);

-}

{-|
Call the hub with the given message and arguments.
This is a low-level routine and users are expected to
use more appropriate routines where available.

This routine includes logging to the "SAMP.StandardProfile.Client"
logger (at present only debug-level information).
-} 
makeCallE :: 
          String       -- ^ url of the hub
       -> String       -- ^ message name
       -> [SAMPValue]  -- ^ message arguments
       -> Err IO SAMPValue     -- ^ response
makeCallE url msg args = do
         dbg $ "Calling message " ++ msg ++ " at " ++ url
         dbg $ "  with args " ++ show args
         rsp <- call url msg (map toValue args)
         dbg $ "Response to " ++ msg
         dbg (show rsp)
         fromValue rsp

-- | Register a client with a hub. See `registerClientE` for a simple
-- | way to register the client and process the return vaues.
registerE :: SAMPInfo     -- ^ hub information
          -> Err IO [SAMPKeyValue] -- ^ Key/value pairs from the registration call.
registerE (sKey,url) =
    makeCallE url "samp.hub.register" [SAMPString sKey]
    >>= fromSValue

sPrivateKey , sHubId , sSelfId :: RString
sPrivateKey = fromJust (toRString "samp.private-key")
sHubId      = fromJust (toRString "samp.hub-id")
sSelfId     = fromJust (toRString "samp.self-id")

-- lookup a RString value from a SAMPKeyValue list
slookup :: (Monad m) => RString -> [SAMPKeyValue] -> Err m RString
slookup k a = case lookup k a of
                Just (SAMPString s) -> return s
                Just x              -> throwError $ "Expected a string for " ++ show k ++ " but found " ++ show x
                _                   -> throwError $ "Unable to find key " ++ show k

-- | Create a `SAMPConnection` record from the hub information and response
-- | from `registerE`.
getClientInfoE :: (Monad m) =>
               SAMPInfo   -- ^ hub information
               -> [SAMPKeyValue] -- ^ response from `registerE`
               -> Err m SAMPConnection
getClientInfoE (sKey,url) ks = SAMPConnection `liftM`
                  return sKey 
                  `ap` return url
                  `ap` slookup sPrivateKey ks
                  `ap` slookup sHubId ks
                  `ap` slookup sSelfId ks

-- | Register the client with the hub and create the `SAMPConnection` record
-- | used for communication.
registerClientE :: SAMPInfo -> Err IO SAMPConnection
registerClientE si = registerE si >>= getClientInfoE si

-- | Unregister the client from the hub.
unregisterE :: SAMPConnection
            -> Err IO ()
unregisterE cl =
    makeCallE (sampHubURL cl) "samp.hub.unregister" [SAMPString (sampPrivateKey cl)]
    >> return ()

sName , sTxt, sHtml, sIcon, sDoc :: RString
sName = fromJust $ toRString "samp.name"
sTxt  = fromJust $ toRString "samp.description.text"
sHtml = fromJust $ toRString "samp.description.html"
sIcon = fromJust $ toRString "samp.icon.url"
sDoc  = fromJust $ toRString "samp.documentaion.url"

-- | Create the key/value pairs used by `declareMetadataE`
-- | for the common metadata settings.
toMetadata :: RString -- ^ A one-word title for the application (samp.name)
           -> Maybe RString -- ^ A short description of the application in plain text (samp.description.text)
           -> Maybe RString -- ^ A description of the application in HTML (samp.description.html)
           -> Maybe RString -- ^ The URL of an icon in png, gif or jpeg format (samp.icon.url)
           -> Maybe RString -- ^ The URL of a documentation web page (samp.documentation.url)
           -> [SAMPKeyValue]
toMetadata m txt html icon doc =
    let f k = fmap $ (,) k . SAMPString
        ms = [f sTxt txt, f sHtml html, f sIcon icon, f sDoc doc]
    in (sName, SAMPString m) : catMaybes ms

-- | Declare the metadata for the client. The metadata is provided as
-- | a list of (key,value) pairs (this is slightly different from the
-- | SAMP API which has the return being a map; here we extract the 
-- | contents of the map). See `toMetadata` for a convenience routine
-- | for setting the common metadata fields.
-- |
-- | This overwrites the existing metadata stored in the hub for the
-- | client. 
declareMetadataE :: SAMPConnection
                 -> [SAMPKeyValue] -- ^ the key/value pairs to declare 
                 -> Err IO ()
declareMetadataE cl ks = 
    makeCallE (sampHubURL cl) "samp.hub.declareMetadata" [SAMPString (sampPrivateKey cl), SAMPMap ks]
    >> return ()

-- | Return the metadata for another client of the SAMP hub as a
-- | list of (key,value) pairs.
getMetadataE :: SAMPConnection
             -> RString -- ^ The id of the SAMP client to query
             -> Err IO [SAMPKeyValue] -- ^ The metadata key/value pairs of the queried client
getMetadataE cl clid =
    makeCallE (sampHubURL cl) "samp.hub.getMetadata" [SAMPString (sampPrivateKey cl), SAMPString clid]
    >>= fromSValue

mtToRS :: MType -> RString
mtToRS = fromJust . toRString . show

-- | Declare the subscriptions for this client. The subscriptions are
-- | given as a list of (key,value) pairs where the keys are the MTypes
-- | of the messages to subscribe to and the value is determined by the
-- | selected `MType`.
-- |
-- | This overwrites the existing subscriptions for the client.
-- | 
-- | See `declareSubscriptionsSimpleE` for the case when the messages
-- | being subscribed to have no parameters.
declareSubscriptionsE :: SAMPConnection
                      -> [(MType, SAMPValue)] -- ^ the messages the client is subscribing to
                      -> Err IO ()
declareSubscriptionsE cl subs =
    let ks = map (CA.first mtToRS) subs
    in makeCallE (sampHubURL cl) "samp.hub.declareSubscriptions" [SAMPString (sampPrivateKey cl), SAMPMap ks]
       >> return ()

-- | Declare the subscriptions for this client. This can be used
-- | if all the messages require no parameters; use 
-- | `declareSubscriptionsE` for the general case.
declareSubscriptionsSimpleE :: SAMPConnection
                            -> [MType] -- ^ the messages the client is subscribing to
                            -> Err IO ()
declareSubscriptionsSimpleE cl mtypes =
    let ks = map (\(n) -> (mtToRS n, SAMPMap [])) mtypes
    in makeCallE (sampHubURL cl) "samp.hub.declareSubscriptions" [SAMPString (sampPrivateKey cl), SAMPMap ks]
       >> return ()

-- | Get the message subscriptions of a client. The subscriptions are
-- | returned as a list of (key,value) pairs.
getSubscriptionsE :: SAMPConnection
                  -> RString -- ^ the name of the client to query
                  -> Err IO [SAMPKeyValue] -- ^ the (key,value) subscriptions of the queried client
getSubscriptionsE cl clid = 
    makeCallE (sampHubURL cl) "samp.hub.getSubscriptions" [SAMPString (sampPrivateKey cl), SAMPString clid]
    >>= fromSValue

-- | Return a list of all the registered clients of the hub - including
-- | itsels - but excluding this client.
getRegisteredClientsE :: SAMPConnection
                      -> Err IO [RString] -- ^ the names of the registered clients
getRegisteredClientsE cl =
    makeCallE (sampHubURL cl) "samp.hub.getRegisteredClients" [SAMPString (sampPrivateKey cl)]
    >>= fromSValue

-- | Return a (key,value) list of all the clients that are subscibed to
-- | a given `MType` (the return value is slightly different from the
-- | SAMP API which has the return being a map; here we extract the 
-- | contents of the map).
-- |
-- | An error occurs if the MType contains a wild card
getSubscribedClientsE :: SAMPConnection
                      -> MType -- ^ the message (it can not contain a wildcard)
                      -> Err IO [SAMPKeyValue]
getSubscribedClientsE cl mtype =
    when (isMTWildCard mtype) (throwError "MType can not contain a wild card when calling getSubscribedClients")
    >> makeCallE (sampHubURL cl) "samp.hub.getSubscribedClients" [SAMPString (sampPrivateKey cl), toSValue mtype]
    >>= fromSValue

-- | Send a message to a given client of the hub and do not
-- | wait for a response.
notifyE :: SAMPConnection
        -> RString -- ^ the name of the client to notify
        -> SAMPMessage -- ^ the message
        -> Err IO ()
notifyE cl clid msg =
    makeCallE (sampHubURL cl) "samp.hub.notify"
          [SAMPString (sampPrivateKey cl), SAMPString clid, toSValue msg]
    >> return ()

-- | Send a message to all clients and get back a list of those
-- | that were sent the message (i.e. are subscribed to it). Note that
-- | just because a client was sent a message does not mean it was successful.
notifyAllE :: SAMPConnection
           -> SAMPMessage -- ^ the message
           -> Err IO [RString] -- ^ the list of clients that were sent the message
notifyAllE cl msg =
    makeCallE (sampHubURL cl) "samp.hub.notifyAll"
          [SAMPString (sampPrivateKey cl), toSValue msg]
    >>= fromSValue

-- | Send a message asynchronously to the recipient. The return value is
-- | the message id given to this communication by the hub.
-- | The client must be callable for this to work (see `setXmlrpcCallbackE`).
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
-- | message type.
-- | The client must be callable for this to work (see `setXmlrpcCallbackE`).
callAllE :: SAMPConnection
         -> RString -- ^ a unique identifier for the communication (the message tag)
         -> SAMPMessage -- ^ the message
         -> Err IO [SAMPKeyValue] -- ^ the key is the name of the client and the value is the message id for that communication
callAllE cl msgtag msg =
    makeCallE (sampHubURL cl) "samp.hub.callAll"
        [SAMPString (sampPrivateKey cl), SAMPString msgtag, toSValue msg]
    >>= fromSValue
    
-- | Send a message to a client and wait for a response. The timeout parameter
-- | controls how long the wait will be before error-ing out (if given).
-- | Unlike `callE` and `callAllE`, the client need not be callable to use
-- | this routine.
callAndWaitE :: SAMPConnection
             -> RString -- ^ the name of the client to contact
             -> SAMPMessage -- ^ the message
             -> Maybe Int -- ^ the maximum timeout to wait, in seconds
             -> Err IO SAMPResponse
callAndWaitE cl clid msg tout = 
    let t = fromMaybe 0 tout
    in makeCallE (sampHubURL cl) "samp.hub.callAndWait"
          [SAMPString (sampPrivateKey cl), SAMPString clid, toSValue msg, toSValue t]
    >>= fromSValue
    
-- | Reply to a message from another client.
replyE :: SAMPConnection
       -> RString -- ^ the message identifier (as returned by the hub from `callE` or `callAllE`)
       -> SAMPResponse -- ^ the response
       -> Err IO ()
replyE cl msgid rsp =
    makeCallE (sampHubURL cl) "samp.hub.reply"
        [SAMPString (sampPrivateKey cl), SAMPString msgid, toSValue rsp]
    >> return ()

-- | Register a XML-RPC endpoint that the client uses to receive
-- | information from the hub. This must be set up before either
-- | `callE` or `callAllE` can be used.
setXmlrpcCallbackE :: SAMPConnection
                   -> RString -- ^ the URL of the end point
                   -> Err IO ()
setXmlrpcCallbackE cl url =
    makeCallE (sampHubURL cl) "samp.hub.setXmlrpcCallback"
        [SAMPString (sampPrivateKey cl), SAMPString url]
    >> return ()

-- | Ping the hub to see if it is running.
-- |
-- | Note that we do not support calling this method without
-- | the private key (taken from the `SAMPConnection` record).
-- | Users who wish to see if a hub is alive and just have a URL
-- | can try using `makeCallE` directly - e.g.
-- |
-- |    makeCallE url "samp.hub.ping" [] >> return ()
-- |
pingE :: SAMPConnection
      -> Err IO ()
pingE cl =
    makeCallE (sampHubURL cl) "samp.hub.ping" [SAMPString (sampPrivateKey cl)]
    >> return ()
