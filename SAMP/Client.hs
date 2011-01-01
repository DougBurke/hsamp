
{-

To do:

  - return Haskell types (list, assoc list, strings) rather than SAMP types?

  - add logging so we can see what is sent and received

The SAMP hubs may return invalid XML-RPC responses on error: I have seen
cases with both JSAMP and SAMPY. Mark has fixed JSAMP but not formally released,
and I have sent a patch to SAMPY to Luigi to fix this, but it does mean we
may have to deal with errors thrown because the error response can not be
deconstructed (so losing the actual error message).

NOTE: 

  errors; could
    - just use String for all errors, perhaps with some 'isThisASAMPError'
      utilities
    - newtype SAMPError a = ErrorT SAMPErrorType IO a    
      and convert Err IO a to SAMPError a at earliest convenience
  in both cases drop using Either

could have several sets of routines;

  ones that return 'Err IO SAMPResponse' or SAMPValue
  ones that return 'IO SAMPValue'

-}

module SAMP.Client (SampClient(..), SampInfo, SAMPValue(..), SAMPResponse(..),
             TransportError,
             showSAMPValue,
             getHubInfo, pingHub,
             registerClient,
             unregisterClient,
             declareMetadata,
             getMetadata,
             declareSubscriptions,
             declareSubscriptionsSimple,
             getSubscriptions,
             getRegisteredClients,
             getSubscribedClients,
             callAndWait,
             notify, notifyAll,
             -- setXmlrpcCallback,

             getHubInfoE, pingHubE,
             registerClientE,
             unregisterClientE,
             declareMetadataE,
             getMetadataE,
             declareSubscriptionsE,
             declareSubscriptionsSimpleE,
             getSubscriptionsE,
             getRegisteredClientsE,
             getSubscribedClientsE,
             callAndWaitE,
             notifyE, notifyAllE,
             setXmlrpcCallbackE

            ) where

import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import qualified System.IO.Strict as S
-- import qualified Data.Map as M
-- import Data.Maybe

import Data.List (isPrefixOf, stripPrefix)
import Data.Char (isDigit)

import qualified Control.Arrow as CA
import Control.Monad (liftM, ap, guard)
-- import Control.Monad.Trans

import Control.Monad.Error (throwError)
import Control.Monad.Trans (liftIO)

import qualified Control.Exception as CE

import System.Environment (getEnv)
-- import System.IO.Error (catch)

-- import System.IO (isDoesNotExistError) invalid hlint suggestion
-- import IO (isDoesNotExistError)
import System.IO.Error (isDoesNotExistError, isDoesNotExistErrorType, ioeGetErrorType)

import SAMP.Types

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

extractHubInfo :: [(String,String)] -> Maybe (SampSecret, SampHubURL)
extractHubInfo alist = do
    secret <- lookup "samp.secret" alist
    url <- lookup "samp.hub.xmlrpc.url" alist
    return (secret, url)

-- This assumes we are on a UNIX environment
--
-- Question: is this the correct catch to use?
-- (it's the Prelude one)
--
getHubInfo :: IO (Maybe SampInfo)
getHubInfo = catch (do
                    fname <- sampLockFile
                    cts <- S.readFile fname
                    let alist = processLockFile cts
                    return $ extractHubInfo alist)
                    (\_ -> return Nothing)

getHubInfoE :: Err IO SampInfo
getHubInfoE = do
    fname <- sampLockFileE
    let emsg = "Unable to find Hub info (" ++ fname ++ ")"
    cts <- checkExists emsg $ S.readFile fname 
    let alist = processLockFile cts
    maybeToM "Unable to read hub info" (extractHubInfo alist)
    
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

{-
Looks like call throws a DoesNotExistError when the hub no longer exists.
-}

-- I decompose the error string to get back the error code and string.
-- Not ideal, and fragile, but simpler than writing my own code to 
-- call an XML-RPC method
--
recreateTransportError :: String -> TransportError
recreateTransportError eMsg = if or [null l, null r, null r2, not ("Error " `isPrefixOf` l), not (all isDigit lCode)]
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

parseGeneralResponseE :: Value -> Err IO SAMPResponse
parseGeneralResponseE = return . SAMPSuccess . toSAMPValue

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
      Just (ValueStruct eStruct) = lookup "samp.error" s

      Just (ValueString eText) = lookup "samp.errortxt" eStruct
      eVals = filter (\(n,_) -> n /= "samp.errortxt") eStruct

      eOut = toSAMPValue (ValueStruct eVals)
      rOut = toSAMPValue result

parseSAMPResponse v = Left $ TransportError 999 $ "Internal error: unrecognized return: " ++ show v

{-
TODO: throw an error if get back samp.error
-}
parseSAMPResponseE :: Value -> Err IO SAMPResponse
parseSAMPResponseE (ValueStruct s) = case status of
                                      Nothing -> throwError $ "Internal error: missing samp.status in response - " ++ show s
                                      Just (ValueString "samp.ok") -> return $ SAMPSuccess rOut
                                      Just (ValueString "samp.error") -> return $ SAMPError eText eOut
                                      Just (ValueString "samp.warning") -> return $ SAMPWarning eText eOut rOut
                                      Just x -> throwError $ "Internal error: unknown samp.status of '" ++ show x ++ "' in " ++ show s
    where
      status = lookup "samp.status" s 
      -- Just (ValueStruct result) = lookup "samp.result" s
      Just result = lookup "samp.result" s
      Just (ValueStruct eStruct) = lookup "samp.error" s

      Just (ValueString eText) = lookup "samp.errortxt" eStruct
      eVals = filter (\(n,_) -> n /= "samp.errortxt") eStruct

      eOut = toSAMPValue (ValueStruct eVals)
      rOut = toSAMPValue result

parseSAMPResponseE v = throwError $ "Internal error: unrecognized return: " ++ show v

-- callHub' and callHub are for methods that return a non-specific return
-- type (i.e. any XML-RPC value given the SAMP restrictions).
--
-- callHubSAMP' and callHubSAMP are for methods that return a SAMP response
--

{-
For the new "error handling" I am going back from having a type for the
error to a string, although I feel I really should have my own transformer,
ErrorT TransportError IO a

-}

cHubE :: (Value -> Err IO SAMPResponse) -> SampHubURL -> String -> [Value] -> Err IO SAMPResponse
-- cHubE f url msg args = call url msg args >>= \m -> liftIO (putStrLn (">>RESPONSE to " ++ msg)) >> liftIO (putStrLn (show m)) >> liftIO (putStrLn "<<RESPONSE") >> f m

cHubE f url msg args = call url msg args >>= f

{-

I could catch exceptions from call, but I don't think it's worth it,
as the caller then loses the knowledge of the failure type.

The following doesn't work anyway.

cHubE f url msg args = do
      let hE :: CE.IOException -> Err IO a
          hE e = if isDoesNotExistError e
                 then throwError $ "Unable to call " ++ msg ++ " at " ++ url ++ " " ++ show e
                 else ioError e
      resp <- call url msg args `CE.catch` hE
      f resp

-}

callHubE' :: SampHubURL -> String -> [Value] -> Err IO SAMPResponse
callHubE' = cHubE parseGeneralResponseE

callHubSAMPE' :: SampHubURL -> String -> [Value] -> Err IO SAMPResponse
callHubSAMPE' = cHubE parseSAMPResponseE

callHubE :: SampHubURL -> Maybe SampSecret -> String -> [Value] -> Err IO SAMPResponse
callHubE url (Just s) method args = callHubE' url method (ValueString s : args)
callHubE url _        method args = callHubE' url method args

callHubSAMPE :: SampHubURL -> Maybe SampSecret -> String -> [Value] -> Err IO SAMPResponse
callHubSAMPE url (Just s) method args = callHubSAMPE' url method (ValueString s : args)
callHubSAMPE url _        method args = callHubSAMPE' url method args

-- XXX TO LOOK AT Do we really need either here or is there a better conversion?
pingHubE' :: SampHubURL -> Err IO Bool
-- pingHubE' u = either (const False) (const True) `liftM` callHubE u Nothing "samp.hub.ping" []
pingHubE' u = do
          resp <- callHubE u Nothing "samp.hub.ping" []
          return $ isSAMPSuccess resp

pingHubE :: Err IO Bool
pingHubE = fmap snd getHubInfoE >>= pingHubE'

-- XXX TODO XXX
--   provide a default set of metadata about the client that can be over-ridden
--   by a call to declareMetadata. Could also add an optional "config" record
--   as a parameter to registerClient
--

registerClientE :: SampInfo -> Err IO SampClient
registerClientE (s,u) = callHubE u (Just s) "samp.hub.register" [] >>= r2c
        where
          r2c :: SAMPResponse -> Err IO SampClient
          r2c (SAMPSuccess (SAMPMap v)) = SampClient `liftM`
                                               return s 
                                               `ap` return u
                                               `ap` xlookup "samp.private-key" v
                                               `ap` xlookup "samp.hub-id" v
                                               `ap` xlookup "samp.self-id" v
          r2c o = throwError $ "Unexpected response from samp.hub.register: " ++ show o

          xlookup :: String -> [(String, SAMPValue)] -> Err IO String
          xlookup k a = case lookup k a of
                          Just (SAMPString s2) -> return s2
                          _                    -> throwError $ "Unable to find key " ++ k

{- OLD CODE -}

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
callHubSAMP url (Just s) method args = callHubSAMP' url method (ValueString s : args)
callHubSAMP url _        method args = callHubSAMP' url method args

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
registerClient :: SampInfo -> IO (Either TransportError SampClient)
registerClient (s,u) = either Left mkClient `liftM` callHub u (Just s) "samp.hub.register" []
        where
          mkClient :: SAMPResponse -> Either TransportError SampClient
          mkClient (SAMPSuccess (SAMPMap v)) = SampClient `liftM`
                                               Right s
                                               `ap` Right u
                                               `ap` xlookup "samp.private-key" v
                                               `ap` xlookup "samp.hub-id" v
                                               `ap` xlookup "samp.self-id" v
          mkClient _ = Left $ TransportError 999 "Unable to process return value of samp.hub.register"

          xlookup k a = case lookup k a of
                          Just (SAMPString s2) -> Right s2
                          _            -> Left $ TransportError 999 ("Unable to find key " ++ k)


-- We do not worry if there is an error un-registering the client
-- but perhaps we should

unregisterClient :: SampClient -> IO ()
unregisterClient sc = doCallHub sc "unregister" [] >> return ()

unregisterClientE :: SampClient -> Err IO ()
unregisterClientE sc = doCallHubE sc "unregister" [] >> return ()

doCallHub :: SampClient -> String -> [Value] -> IO SAMPReturn
doCallHub sc = callHub (sampHubURL sc) (Just (sampPrivateKey sc)) . ("samp.hub." ++)

doCallHubE :: SampClient -> String -> [Value] -> Err IO SAMPResponse
doCallHubE sc = callHubE (sampHubURL sc) (Just (sampPrivateKey sc)) . ("samp.hub." ++)

doCallHubSAMP :: SampClient -> String -> [Value] -> IO SAMPReturn
doCallHubSAMP sc = callHubSAMP (sampHubURL sc) (Just (sampPrivateKey sc)) . ("samp.hub." ++)

doCallHubSAMPE :: SampClient -> String -> [Value] -> Err IO SAMPResponse
doCallHubSAMPE sc = callHubSAMPE (sampHubURL sc) (Just (sampPrivateKey sc)) . ("samp.hub." ++)

-- Name, description, and version: may want a generic one which
-- takes in a map and ensures necessary fields
--
-- XXX TODO XXX
--   clean up, since need to allow arbitrary metadata elements
--

declareMetadata :: SampClient -> String -> String -> String -> IO (Either TransportError ())
declareMetadata sc name desc version = either Left (const (Right ())) `liftM` doCallHub sc "declareMetadata" [mdata]
    where
      mdata = ValueStruct [("samp.name", ValueString name),
                           ("samp.description.text", ValueString desc),
                           ("internal.version", ValueString version)]

declareMetadataE :: SampClient -> String -> String -> String -> [(String, Value)] -> Err IO ()
declareMetadataE sc name desc version opts = doCallHubE sc "declareMetadata" [mdata] >>= isOK >> return ()
    where
      mdata = ValueStruct $ opts ++ [("samp.name", ValueString name),
                             ("samp.description.text", ValueString desc),
                             ("internal.version", ValueString version)]

sampToAssoc :: String -> SAMPResponse -> Err IO [(String, SAMPValue)]
sampToAssoc _ (SAMPSuccess (SAMPMap m)) = return m
sampToAssoc msg e = throwError $ "Unable to parse repsonse for " ++ msg ++ ": " ++ show e

sampToList ::  String -> SAMPResponse -> Err IO [String]
sampToList _ (SAMPSuccess (SAMPList xs)) = return $ map fromSAMPValue xs
sampToList msg e = throwError $ "Unable to parse repsonse for " ++ msg ++ ": " ++ show e


getMetadata :: SampClient -> String -> IO (Either TransportError [(String,SAMPValue)])
getMetadata sc clientId = either Left toAList `liftM` doCallHub sc "getMetadata" [toValue clientId]
    where
      toAList :: SAMPResponse -> Either TransportError [(String, SAMPValue)]
      toAList (SAMPSuccess (SAMPMap m)) = Right m
      toAList _ = Left $ TransportError 999 "Unable to parse repsonse for samp.hub.getMetadata"

getMetadataE :: SampClient -> String -> Err IO [(String,SAMPValue)]
getMetadataE sc clientId = doCallHubE sc "getMetadata" [toValue clientId] >>= sampToAssoc "samp.hub.getMetadata"

-- need to come up with a better type for the subscriptions
-- 
-- We provide a version to allow sending arguments along with each
-- subscription, but provide a simple interface for general use
--
declareSubscriptionsSimple :: SampClient -> [String] -> IO (Either TransportError ())
declareSubscriptionsSimple sc subs = either Left (const (Right ())) `liftM` doCallHub sc "declareSubscriptions" [ValueStruct args]
    where
      args = zip subs (repeat (ValueStruct []))

declareSubscriptionsSimpleE :: SampClient -> [String] -> Err IO ()
declareSubscriptionsSimpleE sc subs = doCallHubE sc "declareSubscriptions" [ValueStruct args] >> return ()
    where
      args = zip subs (repeat (ValueStruct []))

declareSubscriptions :: SampClient -> [(String,[(String,String)])] -> IO (Either TransportError ())
declareSubscriptions sc subs = either Left (const (Right ())) `liftM` doCallHub sc "declareSubscriptions" [ValueStruct args]
    where
      args = map (CA.second alistTovstruct) subs

declareSubscriptionsE :: SampClient -> [(String,[(String,String)])] -> Err IO ()
declareSubscriptionsE sc subs = doCallHubE sc "declareSubscriptions" [ValueStruct args] >> return ()
    where
      args = map (CA.second alistTovstruct) subs

getSubscriptions :: SampClient -> String -> IO (Either TransportError [(String,SAMPValue)])
getSubscriptions sc clientId = either Left toAList `liftM` doCallHub sc "getSubscriptions" [toValue clientId]
    where
      toAList :: SAMPResponse -> Either TransportError [(String, SAMPValue)]
      toAList (SAMPSuccess (SAMPMap m)) = Right m
      toAList _ = Left $ TransportError 999 "Unable to parse response for samp.hub.getSubscriptions"

getSubscriptionsE :: SampClient -> String -> Err IO [(String,SAMPValue)]
getSubscriptionsE sc clientId = doCallHubE sc "getSubscriptions" [toValue clientId] >>= sampToAssoc "samp.hub.getSubscriptions"

getRegisteredClients :: SampClient -> IO (Either TransportError [String])
getRegisteredClients sc = either Left toList `liftM` doCallHub sc "getRegisteredClients" []
    where
      toList :: SAMPResponse -> Either TransportError [String]
      toList (SAMPSuccess (SAMPList xs)) = Right $ map fromSAMPValue xs
      toList _ = Left $ TransportError 999 "Unable to parse response for samp.hub.getRegisteredClients"

getRegisteredClientsE :: SampClient -> Err IO [String]
getRegisteredClientsE sc = doCallHubE sc "getRegisteredClients" [] >>= sampToList "samp.hub.getRegisteredClients"

getSubscribedClients :: SampClient -> String -> IO (Either TransportError [(String,SAMPValue)])
getSubscribedClients sc mType = either Left toAList `liftM` doCallHub sc "getSubscribedClients" [toValue mType]
    where
      toAList :: SAMPResponse -> Either TransportError [(String, SAMPValue)]
      toAList (SAMPSuccess (SAMPMap m)) = Right m
      toAList _ = Left $ TransportError 999 "Unable to parse repsonse for samp.hub.getSubscribedClients"

getSubscribedClientsE :: SampClient -> String -> Err IO [(String,SAMPValue)]
getSubscribedClientsE sc mType = doCallHubE sc "getSubscribedClients" [toValue mType] >>= sampToAssoc "samp.hub.getSubscribedClients"

-- reply = undefined

-- We can not assume v is a string, so use toValue and hope that all is well
-- below
alistTovstruct :: (XmlRpcType a) => [(String, a)] -> Value
alistTovstruct = ValueStruct . map (CA.second toValue)

toSAMPMessage :: (XmlRpcType a) => String -> [(String, a)] -> Value
toSAMPMessage mType params = ValueStruct m
    where
      m = [("samp.mtype", ValueString mType), ("samp.params", alistTovstruct params)]

notify :: SampClient -> String -> String -> [(String,String)] -> IO (Either TransportError ())
notify sc clientId mType params = either Left (const (Right ())) `liftM` doCallHub sc "notify" args
    where
      args = [ValueString clientId, toSAMPMessage mType params]

notifyE :: SampClient -> String -> String -> [(String,String)] -> Err IO ()
notifyE sc clientId mType params = doCallHubE sc "notify" args >> return ()
    where
      args = [ValueString clientId, toSAMPMessage mType params]

-- This is not extensible, in the sense that it doesn't allow other parameters
-- than samp.mtype and samp.params to be specified.
--
notifyAll :: SampClient -> String -> [(String,String)] -> IO (Either TransportError [String])
notifyAll sc mType params = either Left toList `liftM` doCallHub sc "notifyAll" [toSAMPMessage mType params]
    where
      toList :: SAMPResponse -> Either TransportError [String]
      toList (SAMPSuccess (SAMPList xs)) = Right $ map fromSAMPValue xs
      toList _ = Left $ TransportError 999 "Unable to parse response for samp.hub.notifyAll"

notifyAllE :: SampClient -> String -> [(String,String)] -> Err IO [String]
notifyAllE sc mType params = doCallHubE sc "notifyAll" [toSAMPMessage mType params] >>= sampToList "samp.hub.notifyAll"

-- This is not extensible, in the sense that it doesn't allow other parameters
-- than samp.mtype and samp.params to be specified.
--
callAndWait :: SampClient -> String -> String -> [(String,String)] -> Int -> IO SAMPReturn
callAndWait sc clientId mType params timeout = doCallHubSAMP sc "callAndWait" args
    where
      args = [ValueString clientId, toSAMPMessage mType params, ValueString $ show timeout]

callAndWaitE :: SampClient -> String -> String -> [(String,String)] -> Int -> Err IO SAMPResponse
callAndWaitE sc clientId mType params timeout = doCallHubSAMPE sc "callAndWait" args
    where
      args = [ValueString clientId, toSAMPMessage mType params, ValueString $ show timeout]

-- should be done when creating everything; ie SAMPResponse shouldn't have an
-- error condition
--
isOK :: SAMPResponse -> Err IO SAMPResponse
isOK (SAMPError emsg evals) = throwError $ "ERROR: " ++ emsg ++ "\n" ++ show evals
isOK r = return r

setXmlrpcCallbackE :: SampClient -> String -> Err IO ()
setXmlrpcCallbackE sc clientUrl = doCallHubE sc "setXmlrpcCallback" [ValueString clientUrl] >>= isOK >> return ()

{-
unsafeRegisterClient :: String -> IO SampClient
unsafeRegisterClient name = do
    hInfo <- getHubInfo
    let Just hi = hInfo
    scl <- registerClient hi
    let Right sc = scl
    _ <- declareMetadata sc name "Quickly hacked-up client" "0.0.1"
    return sc
    
-}
