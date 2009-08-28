
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
             setClientMetadata
            ) where

import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import qualified System.IO.Strict as S
import qualified Data.Map as M
import Data.Maybe

import qualified Control.Arrow as CA
import qualified Control.Monad as CM
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
-- for other calls you use the private key

-- XXX work out how to send in an array of values
--

callHub :: SampHubURL -> Maybe SampSecret -> String -> [Value] -> IO (Either String Value)
callHub url ms method args = handleError (return . Left)
                             (call url method arglist >>= return . Right)
    where
      arglist = case ms of
                  Just s -> ValueString s : args
                  _ -> args

pingHub' :: SampHubURL -> IO Bool
pingHub' u = callHub u Nothing "samp.hub.ping" [] >>= return . either (\_ -> False) (\_ -> True)

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

registerClient :: (SampSecret, SampHubURL) -> IO (Maybe SampClient)
registerClient (s,u) = callHub u (Just s) "samp.hub.register" [] >>=
                       return . either (\_ -> Nothing) (mkClient)
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

unregisterClient :: SampClient -> IO ()
unregisterClient sc = callHub u (Just s) "samp.hub.unregister" [] >> return ()
    where
      u = sampHubURL sc
      s = sampPrivateKey sc -- this is BAD since callHub needs to be re-written to better reflect this usage

-- Name, description, and version: may want a generic one which
-- takes in a map and ensures necessary fields
--
-- do we want the return to be () or Bool (say?)
--
setClientMetadata :: SampClient -> String -> String -> String -> IO Bool
setClientMetadata sc name desc version = callHub (sampHubURL sc) (Just (sampPrivateKey sc)) "samp.hub.declareMetadata"  [mdata] >>= 
                                         return . either (\_ -> False) (\_ -> True)
    where
      mdata = ValueStruct [("samp.name", ValueString name),
                           ("samp.description.text", ValueString desc),
                           ("internal.version", ValueString version)]

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
    setClientMetadata sc name "Quickly hacked-up client" "0.0.1"
    return sc
    
