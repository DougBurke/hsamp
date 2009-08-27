
-- not currently used {-# LANGUAGE FlexibleContexts #-}

-- may need to use
--    -hide-package transformers
--  (or mtl) to getliftIO and friends working

--
-- SAMP access tests
--

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
    fromValue (ValueString s) = return $ (SAMPString s)
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


-- TODO: when this is called there's a problem with types
pingHub' :: (SampSecret, SampHubURL) -> IO Bool
pingHub' (s,u) = do
    putStrLn "Calling samp.app.ping"
    v <- remote u "samp.app.ping" s :: IO String -- what type should the return be?
    return $ v == "1"

{-
    putStrLn $ "Ping returned " ++ (show v)
    case v of
      ValueString "1" -> return True
      _               -> return False
-}

pingHub :: IO Bool
pingHub = do
    hInfo <- getHubInfo
    case hInfo of
      Just hi -> pingHub' hi
      _       -> return False

{-
JSAMP return from samp.hub.register call

ValueStruct [("samp.hub-id",ValueString "hub"),("samp.self-id",ValueString "c1"),("samp.private-key",ValueString "k:2_wsfxgqdihoxgotve")]

-}

registerClient :: (SampSecret, SampHubURL) -> IO (Maybe SampClient)
registerClient (s,u) = do
    vs <- remote u "samp.hub.register" s :: IO Value
    return $ mkClient vs
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
unregisterClient sc = do
    _ <- remote (sampHubURL sc) "samp.hub.unregister" (sampPrivateKey sc) :: IO Value
    return ()

-- Name, description, and version: may want a generic one which
-- takes in a map and ensures necessary fields
--
setClientMetadata :: SampClient -> String -> String -> String -> IO ()
setClientMetadata sc name desc version = do
   let mdata = [("samp.name", name), ("samp.description.text", desc), ("internal.version", version)]
   v <- remote (sampHubURL sc) "samp.hub.declareMetadata" (sampPrivateKey sc) mdata :: IO Value
   case v of
     ValueString "" -> return ()
     _ -> fail $ "ERROR: samp.hub.declareMetadata failed with " ++ show v

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
    
