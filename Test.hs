
-- not currently used {-# LANGUAGE FlexibleContexts #-}

-- NEED TO USE
--    -hide-package monads-fd


-- may need to use
--    -hide-package transformers
--    -hide-package monads-fd
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

import Data.List (isPrefixOf)
import Data.Char (isDigit)

import qualified Control.Arrow as CA
import Control.Monad (liftM)
import qualified Control.Monad.Error.Class as ME

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

-- this is not a total function!
toSAMPValue :: Value -> SAMPValue
toSAMPValue (ValueString s) = SAMPString s
toSAMPValue (ValueArray as) = SAMPList $ map toSAMPValue as
toSAMPValue (ValueStruct s) = SAMPMap $ map (CA.second toSAMPValue) s

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


data TransportError = TransportError Int String -- ^ XML-RPC error, the first argument is the faultCode and the second the 
                                                -- faultString of the response
                      deriving (Eq, Show)

instance ME.Error TransportError where
    strMsg = TransportError 999

data SAMPResponse = SAMPSuccess SAMPValue -- ^ successful call
                | SAMPError String SAMPValue -- ^ The first argument is the contents of the samp.errortxt,
                                             -- the second argument is a SAMPMap containing any other elements
                                             -- (these are not explicitly named in case new fields are added)
                                             -- This will typically be delivered to the user of the sender application.
                | SAMPWarning String SAMPValue SAMPValue -- ^ amalgum of SAMPError and SAMPSuccess
                     deriving (Eq, Show)

type SAMPReturn = Either TransportError SAMPResponse

-- I decompose the error string to get back the error code and string.
-- Not ideal, and fragile, but simpler than writing my own code to 
-- call an XML-RPC method
--
recreateTransportError :: String -> TransportError
recreateTransportError eMsg = if null l || null r || null r2 || not ("Error " `isPrefixOf` l) || not (all isDigit lCode)
                              then TransportError 999 eMsg
                              else TransportError code msg
    where
      (l,r) = break (==':') eMsg

      lCode = drop 6 l
      code = read lCode

      r2 = tail r
      msg = tail r2

-- For the moment assume there can only be keys of
--   "samp.status", "samp.error", "samp.result"
-- in the response. We also assume the response is correctly
-- typed.
--
-- Actually, should we just convert SAMP types to native Haskell
-- types here?
--

-- Parse a general return value. If the method returns a SAMP Response
-- then use parseSAMPResponse instead.
--
parseGeneralResponse :: Value -> SAMPReturn
parseGeneralResponse = Right . SAMPSuccess . toSAMPValue

-- This is used to parse the return value from a method that
-- returns a SAMP Response value, which is currently only the
-- callAndWait method.
--
parseSAMPResponse :: Value -> SAMPReturn
parseSAMPResponse (ValueStruct s) = case status of
                                      Nothing -> Left $ TransportError 999 $ "Internal error: missing samp.status in response - " ++ show s
                                      Just (ValueString "samp.ok") -> Right $ SAMPSuccess rOut
                                      Just (ValueString "samp.error") -> Right $ SAMPError eText eOut
                                      Just (ValueString "samp.warning") -> Right $ SAMPWarning eText eOut rOut
                                      Just x -> Left $ TransportError 999 $ "Internal error: unknown samp.status of '" ++ show x ++ "' in " ++ show s
    where
      status = lookup "samp.status" s 
      -- Just (ValueStruct result) = lookup "samp.result" s
      Just result = lookup "samp.result" s
      Just (ValueStruct error) = lookup "samp.error" s

      Just (ValueString eText) = lookup "samp.errortxt" error
      eVals = filter (\(n,v) -> n /= "samp.errortxt") error

      eOut = toSAMPValue (ValueStruct eVals)
      rOut = toSAMPValue result

parseSAMPResponse v = Left $ TransportError 999 $ "Internal error: unrecognized return: " ++ show v

-- callHub' and callHub are for methods that return a non-specific return
-- type (i.e. any XML-RPC value given the SAMP restrictions).
--
-- callHubSAMP' and callHubSAMP are for methods that return a SAMP response
--

cHub :: (Value -> SAMPReturn) -> SampHubURL -> String -> [Value] -> IO SAMPReturn
cHub f url msg = handleError (return . Left . recreateTransportError) . liftM f . call url msg

callHub' :: SampHubURL -> String -> [Value] -> IO SAMPReturn
callHub' = cHub parseGeneralResponse

callHubSAMP' :: SampHubURL -> String -> [Value] -> IO SAMPReturn
callHubSAMP' = cHub parseSAMPResponse

callHub :: SampHubURL -> Maybe SampSecret -> String -> [Value] -> IO SAMPReturn
callHub url (Just s) method args = callHub' url method (ValueString s : args)
callHub url _        method args = callHub' url method args

callHubSAMP :: SampHubURL -> Maybe SampSecret -> String -> [Value] -> IO SAMPReturn
callHubSAMP url (Just s) method args = callHub' url method (ValueString s : args)
callHubSAMP url _        method args = callHub' url method args

pingHub' :: SampHubURL -> IO Bool
pingHub' u = either (const False) (const True) `liftM` callHub u Nothing "samp.hub.ping" []

pingHub :: IO Bool
pingHub = do
    hInfo <- getHubInfo
    case hInfo of
      Just (_,url) -> pingHub' url
      _            -> return False

-- XXX TODO XXX
--   provide a default set of metadata about the client that can be over-ridden
--   by a call to declareMetadata. Could also add an optional "config" record
--   as a parameter to registerClient
--
registerClient :: (SampSecret, SampHubURL) -> IO (Either TransportError SampClient)
registerClient (s,u) = either Left mkClient `liftM` callHub u (Just s) "samp.hub.register" []
        where
          mkClient :: SAMPResponse -> Either TransportError SampClient
          mkClient (SAMPSuccess (SAMPMap v)) = do
            pkey <- xlookup "samp.private-key" v
            hid <- xlookup "samp.hub-id" v
            sid <- xlookup "samp.self-id" v
            Right $ SampClient { sampSecret = s,
                                 sampHubURL = u,
                                 sampPrivateKey = pkey,
                                 sampHubId = hid,
                                 sampId = sid
                               }
          mkClient _ = Left $ TransportError 999 "Unable to process return value of samp.hub.register"

          xlookup k a = case lookup k a of
                          Just (SAMPString s) -> Right s
                          _            -> Left $ TransportError 999 ("Unable to find key " ++ k)


-- We do not worry if there is an error un-registering the client
-- but perhaps we should

unregisterClient :: SampClient -> IO ()
unregisterClient sc = callHub u (Just s) "samp.hub.unregister" [] >> return ()
    where
      u = sampHubURL sc
      s = sampPrivateKey sc -- this is BAD since callHub needs to be re-written to better reflect this usage

doCallHub :: SampClient -> String -> [Value] -> IO SAMPReturn
doCallHub sc msg args = callHub (sampHubURL sc) (Just (sampPrivateKey sc)) ("samp.hub."++msg) args

doCallHubSAMP :: SampClient -> String -> [Value] -> IO SAMPReturn
doCallHubSAMP sc msg args = callHubSAMP (sampHubURL sc) (Just (sampPrivateKey sc)) ("samp.hub."++msg) args

-- Name, description, and version: may want a generic one which
-- takes in a map and ensures necessary fields
--
-- XXX TODO XXX
--   clean up, since need to allow arbitrary metadata elements
--
declareMetadata :: SampClient -> String -> String -> String -> IO SAMPReturn
declareMetadata sc name desc version = doCallHub sc "declareMetadata" [mdata]
    where
      mdata = ValueStruct [("samp.name", ValueString name),
                           ("samp.description.text", ValueString desc),
                           ("internal.version", ValueString version)]

-- Qus: For map return values, would it be good to encode this
-- in the type? If so would need a specific map type which is
-- probably a bad idea
--

getMetadata :: SampClient -> String -> IO SAMPReturn
getMetadata sc clientId = doCallHub sc "getMetadata" [toValue clientId]

-- need to come up with a better type for the subscriptions
-- 
-- We provide a version to allow sending arguments along with each
-- subscription, but provide a simple interface for general use
--
declareSubscriptionsSimple :: SampClient -> [String] -> IO SAMPReturn
declareSubscriptionsSimple sc subs = doCallHub sc "declareSubscriptions" [ValueStruct args]
    where
      args = zip subs (repeat (ValueStruct []))

declareSubscriptions :: SampClient -> [(String,[(String,String)])] -> IO SAMPReturn
declareSubscriptions sc subs = doCallHub sc "declareSubscriptions" [ValueStruct args]
    where
      args = map (\(name,alist) -> (name, alistTovstruct alist)) subs

getSubscriptions :: SampClient -> String -> IO SAMPReturn
getSubscriptions sc clientId = doCallHub sc "getSubscriptions" [toValue clientId]

getRegisteredClients :: SampClient -> IO SAMPReturn
getRegisteredClients sc = doCallHub sc "getRegisteredClients" []

getSubscribedClients :: SampClient -> String -> IO SAMPReturn
getSubscribedClients sc mType = doCallHub sc "getSubscribedClients" [toValue mType]

reply = undefined

-- We can not assume v is a string, so use toValue and hope that all is well
-- below
alistTovstruct = ValueStruct . map (\(n,v) -> (n, toValue v))

toSAMPMessage mType params = ValueStruct m
    where
      m = [("samp.mtype", ValueString mType), ("samp.params", alistTovstruct params)]

notify :: SampClient -> String -> String -> [(String,String)] -> IO SAMPReturn
notify sc clientId mType params = doCallHub sc "notify" args
    where
      args = [ValueString clientId, toSAMPMessage mType params]

-- This is not extensible, in the sense that it doesn't allow other parameters
-- than samp.mtype and samp.params to be specified.
--
notifyAll :: SampClient -> String -> [(String,String)] -> IO SAMPReturn
notifyAll sc mType params = doCallHub sc "notifyAll" [toSAMPMessage mType params]

-- This is not extensible, in the sense that it doesn't allow other parameters
-- than samp.mtype and samp.params to be specified.
--
callAndWait :: SampClient -> String -> String -> [(String,String)] -> Int -> IO SAMPReturn
callAndWait sc clientId mType params timeout = doCallHubSAMP sc "callAndWait" args
    where
      args = [ValueString clientId, toSAMPMessage mType params, ValueString $ show timeout]

unsafeRegisterClient :: String -> IO SampClient
unsafeRegisterClient name = do
    hInfo <- getHubInfo
    let Just hi = hInfo
    scl <- registerClient hi
    let Right sc = scl
    declareMetadata sc name "Quickly hacked-up client" "0.0.1"
    return sc
    
