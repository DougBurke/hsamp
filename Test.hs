
-- not currently used {-# LANGUAGE FlexibleContexts #-}

-- may need to use
--    -hide-package transformers
--  (or mtl) to getliftIO and friends working

--
-- SAMP access tests
--

{-

I think we will want some sort of SampProfile typeclass - e.g.

class SampProfile a where
  getProfile :: IO a -- is this possible
  sendMessage :: a -> String -> [(String,String)] -> IO () -- not sure about the return value
  ... etc

and then 

registerClient :: (SampProfile a, SampClient b) => a -> ... -> IO b
  
hmmm, this looks a bit messy

I think the SampProfile shoule be able to

  ping the hub
  register a client
  send a message

  notify options
  .... other stuff ....

Except that this suggests it's really a SampProfileClient.

Since the client registration returns information we need, I think it does look like there's
a SampProfile and a SampClient.

-}

{-
The SAMP hubs may return invalid XML-RPC responses on error: I have seen
cases with both JSAMP and SAMPY. Mark has fixed JSAMP but not formally released,
and I have sent a patch to SAMPY to Luigi to fix this, but it does mean we
may have to deal with errors thrown because the error response can not be
deconstructed (so losing the actual error message).

-}

module Test (SampClient,
             getHubInfo, pingHub,
             registerClient,
             unregisterClient,
             declareMetadata
            ) where

import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import qualified System.IO.Strict as S
import qualified Data.Map as M
import Data.Maybe

import qualified Control.Arrow as CA
import Control.Monad (liftM)
-- import Control.Monad.Trans

import System.Environment (getEnv)
import System.IO.Error (catch)

type SampSecret = String
type SampHubURL = String

sampLockFile :: IO FilePath
sampLockFile = do
    home <- getEnv "HOME"
    return $ home ++ "/.samp"

-- As the SAMP control file may well be written using Windowswor
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

isLineTerminator c = c == '\r' || c == '\n'

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
        step l a = CA.second tail (break (== '=') l) : a

extractHubInfo :: [(String,String)] -> Maybe (SampSecret, SampHubURL)
extractHubInfo alist = do
    secret <- lookup "samp.secret" alist
    url <- lookup "samp.hub.xmlrpc.url" alist
    return (secret, url)

-- This assumes we are on a UNIX environment
getHubInfo :: IO (Maybe (SampSecret, SampHubURL))
getHubInfo = catch (do
                    fname <- sampLockFile
                    cts <- S.readFile fname
                    let alist = processLockFile cts
                    return $ extractHubInfo alist)
                    (\_ -> return Nothing)

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

-- should we have a "restricted string" class that is limited to
-- ASCII characters with hex codes 09,0a,0d or 20 to 7f ?
--
data SAMPValue =
    SAMPString String |
    SAMPList [SAMPValue] |
    SAMPMap   [(String, SAMPValue)]
  deriving (Eq, Show)

-- is the following sensible?

instance XmlRpcType SAMPValue where
    toValue (SAMPString s) = ValueString s
    toValue (SAMPList a)   = ValueArray $ map toValue a
    toValue (SAMPMap m)    = ValueStruct $ map (CA.second toValue) m

    getType (SAMPString _) = TString
    getType (SAMPList _) = TArray
    getType (SAMPMap _) = TStruct

    -- TODO: need the fromValues for arrays and structs otherwise this
    --  is rather pointless
    fromValue (ValueString s) = return $ SAMPString s
    -- fromValue (ValueArray a)  = return $ SAMPList $ mapM fromValue a
    -- fromValue (ValueStruct a)  = return $ SAMPMap $ mapM (CA.second fromValue) a
    fromValue v = fail $ "Unable to convert '" ++ show v ++ "' to a SAMPValue"

type SampId = String
type SampPrivateKey = String

-- How can we make it so that this data is invalid once we unregister
-- a client? Any calls with a previously valid structure will fail,
-- unless there's some fancy-schmancy type class magic to avoid it,
-- which I'm sure there is.
--
data SampClient = SampClient {
                               sampSecret :: SampSecret,
                               sampHubURL :: SampHubURL,
                               sampPrivateKey :: SampPrivateKey,
                               sampHubId :: SampId,
                               sampId :: SampId
                             } deriving (Eq, Show)


-- Just in case we want the error value, we wrap the call method
-- rather than use remote. This is specialized to the standard SAMP
-- profile. Once I get the SAMPValue data type to be an instance of
-- XmlRpcType (ie get fromValue working) then this can return
-- IO (Either String SAMPValue).
--

-- XXX TODO: re-write since SampSecret is only used when registering;
-- for other calls you use the private key; so move to callHub' and then
-- some wrappers around this

-- XXX work out how to send in an array of values
--

callHub' :: SampHubURL -> String -> [Value] -> IO (Either String Value)
callHub' url method = handleError (return . Left) . liftM Right . call url method

-- An alternative to callHub' which is not specific to the Either monad

cH url method = handleError (return . fail) . liftM return . call url method

callHub :: SampHubURL -> Maybe SampSecret -> String -> [Value] -> IO (Either String Value)
callHub url (Just s) method args = callHub' url method (ValueString s : args)
callHub url _        method args = callHub' url method args

pingHub' :: SampHubURL -> IO Bool
pingHub' u = either (const False) (const True) `liftM` callHub u Nothing "samp.hub.ping" []

pingHub :: IO Bool
pingHub = do
    hInfo <- getHubInfo
    case hInfo of
      Just (_,url) -> pingHub' url
      _            -> return False

{-
JSAMP return from samp.hub.register call

ValueStruct [("samp.hub-id",ValueString "hub"),("samp.self-id",ValueString "c1"),("samp.private-key",ValueString "k:2_wsfxgqdihoxgotve")]

-}

-- XXX TODO XXX
--   provide a default set of metadata about the client that can be over-ridden
--   by a call to declareMetadata. Could also add an optional "config" record
--   as a parameter to registerClient
--
registerClient :: (SampSecret, SampHubURL) -> IO (Maybe SampClient)
registerClient (s,u) = either (const Nothing) mkClient `liftM` callHub u (Just s) "samp.hub.register" []
        where
            mkClient (ValueStruct v) = do
                pkey <- xlookup "samp.private-key" v
                hid <- xlookup "samp.hub-id" v
                sid <- xlookup "samp.self-id" v
                return SampClient { sampSecret = s,
                                    sampHubURL = u,
                                    sampPrivateKey = pkey,
                                    sampHubId = hid,
                                    sampId = sid
                                    -- sampPrivateKey = fromValue pkey,
                                    -- sampHubId = fromValue hid,
                                    -- sampId = fromValue sid
                                  }
            mkClient _ = Nothing

            xlookup k a = do
                v <- lookup k a
                case v of
                    ValueString s -> Just s
                    _ -> Nothing


-- We do not worry if there is an error un-registering the client
-- but perhaps we should

unregisterClient :: SampClient -> IO ()
unregisterClient sc = callHub u (Just s) "samp.hub.unregister" [] >> return ()
    where
      u = sampHubURL sc
      s = sampPrivateKey sc -- this is BAD since callHub needs to be re-written to better reflect this usage

-- call the hub with the given arguments, and return either
-- Nothing or Just Value (in IO)
--
doCallHub :: SampClient -> String -> [Value] -> IO (Maybe Value)
doCallHub sc msg args = either (const Nothing) Just
                        `liftM`
                        callHub (sampHubURL sc) (Just (sampPrivateKey sc)) ("samp.hub."++msg) args

doCallHubBool :: SampClient -> String -> [Value] -> IO Bool
doCallHubBool sc msg args = either (const False) (const True)
                            `liftM`
                            callHub (sampHubURL sc) (Just (sampPrivateKey sc)) ("samp.hub."++msg)  args

doCallHubEither :: SampClient -> String -> [Value] -> IO (Either String Value)
doCallHubEither sc msg args = callHub (sampHubURL sc) (Just (sampPrivateKey sc)) ("samp.hub."++msg) args

-- Name, description, and version: may want a generic one which
-- takes in a map and ensures necessary fields
--
-- XXX TODO XXX
--   clean up, since need to allow arbitrary metadata elements
--
declareMetadata :: SampClient -> String -> String -> String -> IO Bool
declareMetadata sc name desc version = doCallHubBool sc "declareMetadata" [mdata]
    where
      mdata = ValueStruct [("samp.name", ValueString name),
                           ("samp.description.text", ValueString desc),
                           ("internal.version", ValueString version)]

-- Qus: For map return values, would it be good to encode this
-- in the type? If so would need a specific map type which is
-- probably a bad idea
--

getMetadata :: SampClient -> String -> IO (Maybe Value)
getMetadata sc clientId = doCallHub sc "getMetadata" [toValue clientId]

-- need to come up with a better type for the subscriptions
-- 
-- We provide a version to allow sending arguments along with each
-- subscription, but provide a simple interface for general use
--
declareSubscriptionsSimple :: SampClient -> [String] -> IO Bool
declareSubscriptionsSimple sc subs = doCallHubBool sc "declareSubscriptions" [ValueStruct args]
    where
      args = zip subs (repeat (ValueStruct []))

declareSubscriptions :: SampClient -> [(String,[(String,String)])] -> IO Bool
declareSubscriptions sc subs = doCallHubBool sc "declareSubscriptions" [ValueStruct args]
    where
      args = map (\(name,alist) -> (name, alistTovstruct alist)) subs

-- for testing
declareSubscriptions' :: SampClient -> [(String,[(String,String)])] -> IO (Either String Value)
declareSubscriptions' sc subs = doCallHubEither sc "declareSubscriptions" [ValueStruct args]
    where
      args = map (\(name,alist) -> (name, alistTovstruct alist)) subs

getSubscriptions :: SampClient -> String -> IO (Maybe Value)
getSubscriptions sc clientId = doCallHub sc "getSubscriptions" [toValue clientId]

getRegisteredClients :: SampClient -> IO (Maybe Value)
getRegisteredClients sc = doCallHub sc "getRegisteredClients" []

getSubscribedClients :: SampClient -> String -> IO (Maybe Value)
getSubscribedClients sc mType = doCallHub sc "getSubscribedClients" [toValue mType]

reply = undefined

-- We can not assume v is a string, so use toValue and hope that all is well
-- below
alistTovstruct = ValueStruct . map (\(n,v) -> (n, toValue v))

toSAMPMessage mType params = ValueStruct m
    where
      m = [("samp.mtype", ValueString mType), ("samp.params", alistTovstruct params)]

-- Bool probably isn't sufficient for a return type because the routine can
-- fail if the client is not registered to receive the given message type.
-- We could return 'Either String Bool' but then this should be done for all
-- types
--
notify :: SampClient -> String -> String -> [(String,String)] -> IO Bool
notify sc clientId mType params = doCallHubBool sc "notify" args
    where
      args = [ValueString clientId, toSAMPMessage mType params]

notify' :: SampClient -> String -> String -> [(String,String)] -> IO (Either String Value) -- TODO change Value to Bool
notify' sc clientId mType params = doCallHubEither sc "notify" args
    where
      args = [ValueString clientId, toSAMPMessage mType params]

-- This is not extensible, in the sense that it doesn't allow other parameters
-- than samp.mtype and samp.params to be specified.
--
notifyAll :: SampClient -> String -> [(String,String)] -> IO (Maybe Value)
notifyAll sc mType params = doCallHub sc "notifyAll" [toSAMPMessage mType params]


-- This is not extensible, in the sense that it doesn't allow other parameters
-- than samp.mtype and samp.params to be specified.
--
callAndWait :: SampClient -> String -> String -> [(String,String)] -> Int -> IO (Maybe Value)
callAndWait sc clientId mType params timeout = do
  ans <- callAndWait' sc clientId mType params timeout
  return $ case ans of
             Left _ -> Nothing
             Right x -> Just x

callAndWait' :: SampClient -> String -> String -> [(String,String)] -> Int -> IO (Either String Value)
callAndWait' sc clientId mType params timeout = doCallHubEither sc "callAndWait" args
    where
      args = [ValueString clientId, toSAMPMessage mType params, ValueString $ show timeout]

{-   
   # Store metadata in hub for use by other applications.
   map metadata = ("samp.name" -> "dummy",
                   "samp.description.text" -> "Test Application",
                   "dummy.version" -> "0.1-3");
   hub.xmlrpcCall("samp.hub.declareMetadata", private-key, metadata);
-}

unsafeRegisterClient :: String -> IO SampClient
unsafeRegisterClient name = do
    hInfo <- getHubInfo
    let Just hi = hInfo
    scl <- registerClient hi
    let Just sc = scl
    declareMetadata sc name "Quickly hacked-up client" "0.0.1"
    return sc
    
