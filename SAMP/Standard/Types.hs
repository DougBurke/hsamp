{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

{-
Types for the SAMP Standard Profile modules.

NEED TO USE
   -hide-package monads-fd
is this still true?

-}

module SAMP.Standard.Types (
       SAMPConnection(..), SAMPInfo,

       Err, handleError, -- from XmlRpc

       RString, emptyRString, toRString, toRStringE, fromRString, asIntegral, asFloating, asBool,
       MType, toMType, toMTypeE, fromMType, isMTWildCard,

       SAMPType(..), SAMPValue(..), SAMPKeyValue, toSAMPKeyValue, fromSAMPKeyValue,
       toSAMPValue, showSAMPValue,

       SAMPResponse, -- toSAMPResponse,
       getSAMPResponseResult, getSAMPResponseError, getSAMPResponseErrorTxt,
       getSAMPResponseExtra,
       isSAMPSuccess, isSAMPError, isSAMPErrorOnly, isSAMPWarning,

       SAMPMessage, toSAMPMessage, getSAMPMessageType, getSAMPMessageParams,
       getSAMPMessageExtra,

       SAMPMethodCall(..),
       SAMPMethodResponse(..),
       parseSAMPCall, parseSAMPResponse,
       renderSAMPCall, renderSAMPResponse

       ) where

import Control.Monad.Error (MonadError, throwError)
import Control.Monad (forM_, liftM, guard, when)

import Network.XmlRpc.Internals

import Data.List (intercalate)
import Data.List.Split (splitOn, splitOneOf)
import Data.Char (chr, isDigit)
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy.Char8 as L

type SAMPInfo = (RString, String)

rchars :: String
rchars = map chr $ [0x9, 0xa, 0xd] ++ [0x20 .. 0x7f]

isRChar :: Char -> Bool
isRChar = (`elem` rchars)

{-|
A "restricted string" class that is limited to
ASCII characters with hex codes 09,0a,0d or 20 to 7f
as these are the only characters supproted by SAMP.

RStrings can be empty.

They are currently un-used by the interface.

TODO:

  - add a Read instance

The type conversions below use the following BNF productions:

  <digit>         ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
  <digits>        ::= <digit> | <digits> <digit>
  <float-digits>  ::= <digits> | <digits> "." | "." <digits> | <digits> "." <digits>
  <sign>          ::= "+" | "-"

-}

data RString = RS String deriving Eq

instance Show RString where
    show (RS s) = s

emptyRString :: RString
emptyRString = RS ""

instance XmlRpcType RString where
    toValue (RS s) = toValue s
    fromValue (ValueString s) = toRStringE s
    fromValue x = fail $ "Unable to convert to a SAMP string from " ++ show x

    getType _ = TString

-- | Create a `RString` from a normal `String`.
toRString :: String -> Maybe RString
toRString s = if all isRChar s then Just (RS s) else Nothing

-- | Create a `RString` from a normal `String`.
toRStringE :: (Monad m) => String -> Err m RString
toRStringE s = maybeToM ("Unable to convert '" ++ s ++ "' to a SAMP string") (toRString s)

{-
-- an unsafe conversion
forceRS :: String -> RString
forceRS = fromJust . toRString
-}

-- | Extract the contents of the `RString`.
fromRString :: RString -> String
fromRString (RS s) = s

{-|
 Convert an `RString` to an `Integral` value using the rule:

    <SAMP int> ::= [ <sign> ] <digits>
-}
asIntegral :: (Read a, Integral a) => RString -> Maybe a
asIntegral (RS []) = Nothing
asIntegral (RS s@(x:xs)) | x == '-'  = fmap negate (conv xs)
                    | x == '+'  = conv xs
                    | otherwise = conv s
    where
      conv :: (Read a) => String -> Maybe a
      conv [] = Nothing
      conv vs = if all isDigit vs then Just (read vs) else Nothing

-- | Convert an `RString` to a `Floating` value using the rule:
-- |    <SAMP float> ::= [ <sign> ] <float-digits> [ "e" | "E" [ <sign> ] <digits> ]
-- |
asFloating :: (Read a, Floating a) => RString -> Maybe a
asFloating (RS []) = Nothing
asFloating (RS s@(x:xs)) | x == '-'  = fmap negate (conv xs)
                    | x == '+'  = conv xs
                    | otherwise = conv s
    where
      iconv :: String -> Maybe Integer
      iconv is = if all isDigit is then Just (read is) else Nothing

      -- this seems as if I'm complicating it a bit
      conv :: (Read a, Floating a) => String -> Maybe a
      conv [] = Nothing
      conv vs = do
           let sv = splitOneOf "eE" vs
           (ms,es) <- case sv of
                        [m1] -> return (m1, "0")
                        [m2, e2] -> return (m2, e2)
                        _ -> Nothing
           let sm = splitOn "." ms
           (lm,rm) <- case sm of
                    [l1] -> return (l1, "0")
                    [l2,r2] -> return (if l2 == "" then "0" else l2,
                                       if r2 == "" then "0" else r2)
                    _ -> Nothing
           e <- case es of
                  '+':ess -> iconv ess
                  '-':ess -> fmap negate (iconv ess)
                  _ -> iconv es

           return $ read (lm ++ "." ++ rm) ** fromIntegral e

{-
We could just check whether the string is == "0" for False,
True otherwise, but then "2.3e4" and "foo" would be treated as
True when the specification explicitly says to compare as
integer values.
-}

-- | Convert an `RString` into a `Bool`.
-- | The rule is
-- |    <SAMP boolean> ::= "0" | "1"
-- | although any integer value other than 0 can be used for `True`.
-- |
asBool :: RString -> Maybe Bool
asBool rs = fmap (/=0) (asIntegral rs :: Maybe Integer)

{-|

A SAMP MType.

They follow the following NBF productions:

  <mchar> ::= [0-9a-z] | "-" | "_"
  <atom>  ::= <mchar> | <atom> <mchar>
  <mtype> ::= <atom> | <mtype> "." <atom>

We also allow wildcards in certain cases, which is

  <msub>  ::= "*" | <mtype> "." "*"

TODO:
  add a Read instance
-}

data MType = MT [String] Bool deriving Eq

{-
keeping as [String] rather than String is over kill
-}

instance Show MType where
    show (MT xs False) = intercalate "." xs
    show (MT xs True)  = intercalate "." (xs ++ ["*"])

instance SAMPType MType where
    toSValue = SAMPString . RS . show

    fromSValue (SAMPString s) = toMTypeE $ fromRString s
    fromSValue x = throwError $ "Expected a string, sent " ++ show x

instance XmlRpcType MType where
    toValue = toValue . show

    fromValue (ValueString s) = toMTypeE s
    fromValue x = throwError $ "Unable to convert to a SAMP MType from " ++ show x

    getType _ = TString

mtchars :: String
mtchars = ['a' .. 'z'] ++ "-_" ++ map chr [0..9]

isMTChar :: Char -> Bool
isMTChar = (`elem` mtchars)

-- | Create a `MType`. This includes wild cards such as
-- | "*" or "file.event.*".
-- |
toMType :: String -> Maybe MType
toMType [] = Nothing
toMType xs = do
        let terms = splitOn "." xs
            flag = last terms == "*"
            tocheck = if flag then init terms else terms
        forM_ tocheck $ guard . all (/='*')
        guard (all (not.null) tocheck)
        forM_ tocheck $ guard . all isMTChar
        return $ MT tocheck flag

-- | Create a `MType`. This includes wild cards such as
-- | "*" or "file.event.*".
-- |
toMTypeE :: (Monad m) => String -> Err m MType
toMTypeE mt = maybeToM ("Unable to convert '" ++ mt ++ "' to a SAMP MType") (toMType mt)

-- | Extract the contents of the `MType`.
fromMType :: MType -> String
fromMType = show

-- | Is this a wild card `MType`?
isMTWildCard :: MType -> Bool
isMTWildCard (MT _ f) = f

-- | This represents a key,value pair stored in a SAMP map.
type SAMPKeyValue = (RString, SAMPValue)

{-
XXX TODO: write in applicative style
-}

{-
The following routines could be made generic - ie use XmlRpcType a rather
than force Value, but need to look at to see if it is worth it.
-}

-- | Convert a (String, Value) tuple into (RString, SAMPValue)
toSAMPKeyValue :: (Monad m) => (String, Value) -> Err m SAMPKeyValue
toSAMPKeyValue (n,v) = toRStringE n >>= \ns -> fromValue v >>= \vs -> return (ns, vs)

-- | Convert a (RString, SAMPValue) tuple into (String, Value).
fromSAMPKeyValue :: SAMPKeyValue -> (String, Value)
fromSAMPKeyValue (n,v) = (fromRString n, toValue v)

{-
Remove the listed keys from the keylist.
-}
cleanKeys :: [RString] -> [SAMPKeyValue] -> [SAMPKeyValue]
cleanKeys ks = filter (\(k,_) -> k `notElem` ks)

cleanXKeys :: [String] -> [(String,Value)] -> [(String,Value)]
cleanXKeys ks = filter (\(k,_) -> k `notElem` ks)

{- | The SAMP profile supports three basic data types,
a string, a list or a map (keys are strings and they
can contain a SAMP data type).

Strings are restricted to the `RString` class; they may
be used to serialize numeric types, as defined by the
message (ie `MType`). There are several routines for
converting a `RString` to a numeric type following the
syntax given in the SAMP recommendation.
-}

data SAMPValue =
    SAMPString RString
  | SAMPList [SAMPValue]
  | SAMPMap [SAMPKeyValue]
  deriving (Eq, Show)

instance XmlRpcType SAMPValue where
    toValue (SAMPString s) = toValue s
    toValue (SAMPList xs) = ValueArray $ map toValue xs
    toValue (SAMPMap xs) = ValueStruct [(fromRString n, toValue v) | (n,v) <- xs]

    fromValue (ValueString s) = fmap SAMPString $ toRStringE s
    fromValue (ValueArray xs) = liftM SAMPList $ mapM fromValue xs
    fromValue (ValueStruct xs) = liftM SAMPMap $ mapM toSAMPKeyValue xs
    fromValue x = fail $ "Unable to convert to SAMP Value from " ++ show x

    -- it's not obvious whether the argument is meant to be a
    -- "phantom type" here, so there realy should be only one
    -- type as an answer.
    --
    getType (SAMPString _) = TString
    getType (SAMPList _) = TArray
    getType (SAMPMap _) = TStruct

-- | Convert a `SAMPValue` to a displayable string.
showSAMPValue :: SAMPValue -> String
showSAMPValue (SAMPString s) = show s
showSAMPValue (SAMPList xs) = concat ["[", intercalate "," (map showSAMPValue xs), "]"]
showSAMPValue (SAMPMap ms) = concat ["{", intercalate "," vals, "}"]
         where
             vals = map (\(n,v) -> concat [show n, " -> ", showSAMPValue v]) ms

-- | This is a simplified form of the `fromValue` function of the
-- | `XmlRpcType` type.
toSAMPValue :: Value -> Maybe SAMPValue
toSAMPValue vs = handleError (const Nothing) (fromValue vs)

-- | Conversion routines for `SAMPValue` values.
class SAMPType a where
    toSValue :: a -> SAMPValue
    fromSValue :: (Monad m) => SAMPValue -> Err m a

    -- getSType :: a -> Type

instance SAMPType RString where
    toSValue = SAMPString
    fromSValue (SAMPString s) = return s
    fromSValue x = throwError $ "Expected a string, sent " ++ show x

instance SAMPType Bool where
    toSValue True = SAMPString $ RS "1"
    toSValue _    = SAMPString $ RS "0"

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to a Bool.") (asBool s)
    fromSValue x = throwError $ "Expected a string but sent " ++ show x

instance SAMPType Int where
    toSValue = SAMPString . fromJust . toRString . show

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to an Int.") (asIntegral s)
    fromSValue x = throwError $ "Expected a string but sent " ++ show x

instance SAMPType Integer where
    toSValue = SAMPString . fromJust . toRString . show

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to an Integer.") (asIntegral s)
    fromSValue x = throwError $ "Expected a string but sent " ++ show x

instance SAMPType [SAMPKeyValue] where
    toSValue = SAMPMap
    fromSValue (SAMPMap xs) = return xs
    fromSValue x = throwError $ "Expected a SAMP map, sent " ++ show x

instance (SAMPType a) => SAMPType [a] where
    toSValue = SAMPList . map toSValue
    fromSValue (SAMPList xs) = mapM fromSValue xs
    fromSValue x = throwError $ "Expected a SAMP list, sent " ++ show x

{-
TODO: need to encode a float using the correct rules.

instance SAMPType Float where
    toSValue = SAMPString . fromJust . toRString . show

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to an Integer.") (asIntegral s)
    fromSValue x = throwError $ "Expected a string but sent " ++ show x
-}

-- How can we make it so that this data is invalid once we unregister
-- a client? Any calls with a previously valid structure will fail,
-- unless there's some fancy-schmancy type class magic to avoid it,
-- which I'm sure there is; e.g. a phantom type or the use of
-- monadic regions.
--
data SAMPConnection = SAMPConnection {
     sampSecret :: RString,
     sampHubURL :: String,
     sampPrivateKey :: RString,
     sampHubId :: RString,
     sampId :: RString
     } deriving (Eq, Show)

-- | The response from a client to a SAMP call. Use the
-- | `isSAMPSuccess`, `isSAMPError` and `isSAMPWarning` routines
-- | to query the status of the response.
-- | 
data SAMPResponse =
    SR (Maybe [SAMPKeyValue]) (Maybe (RString, [SAMPKeyValue])) [SAMPKeyValue]
    deriving (Eq, Show)

instance SAMPType SAMPResponse where
    toSValue (SR (Just vs) Nothing ex) = 
        SAMPMap $ [(sStatus, sOkVal), (sResult, SAMPMap vs)] ++ ex
    toSValue (SR Nothing (Just (emsg,es)) ex) =
        SAMPMap $ [(sStatus, sErrVal), (sError, SAMPMap nvs)] ++ ex
          where
            nvs = (sErrorTxt, SAMPString emsg) : es
    toSValue (SR (Just vs) (Just (emsg,es)) ex) =
        SAMPMap $ [(sStatus, sWarnVal), (sResult, SAMPMap vs),
                   (sError, SAMPMap nvs)] ++ ex
          where
            nvs = (sErrorTxt, SAMPString emsg) : es
    toSValue x = error $ "Invalid SAMPResponse: " ++ show x

    fromSValue (SAMPMap xs) = do
        ss <- fmap fromRString $ getKey sStatus xs
        case ss of
            "samp.ok" -> do
                fs <- getKey sResult xs
                let nxs = cleanKeys [sStatus, sResult] xs
                return $ SR (Just fs) Nothing nxs

            "samp.error" -> do
                evals <- getKey sError xs
                emsg <- getKey sErrorTxt evals
                let nes = cleanKeys [sErrorTxt] evals
                    nxs = cleanKeys [sStatus, sError] xs
                return $ SR Nothing (Just (emsg,nes)) nxs

            "samp.warning" -> do
                fs <- getKey sResult xs
                evals <- getKey sError xs
                emsg <- getKey sErrorTxt evals
                let nes = cleanKeys [sErrorTxt] evals
                    nxs = cleanKeys [sStatus, sResult, sError] xs
                return $ SR (Just fs) (Just (emsg,nes)) nxs

            _ -> throwError $ "Unexpected samp.status of " ++ show ss

    fromSValue x = throwError $ "Expected a SAMP map but sent " ++ show x

{-
TODO: write 
-- | Constructor for a `SAMPResponse`.
-- toSAMPResponse :: MType -- ^ The `MType` of the message (this is the "samp.mtype" key).
--               -> [SAMPKeyValue]  -- ^ The parameters for the message (this is the "samp.params" key).
--               -> [SAMPKeyValue]  -- ^ Any other key/values in the message.
--               -> SAMPMessage
toSAMPResponse = undefined
-}

sStatus , sResult , sError , sErrorTxt :: RString
sStatus = RS "samp.status"
sResult = RS "samp.result"
sError  = RS "samp.error"
sErrorTxt = RS "samp.errortxt"

sampOK , sampError , sampWarning :: String
sampOK = "samp.ok"
sampError = "samp.error"
sampWarning = "samp.warning"

sOkVal , sErrVal , sWarnVal :: SAMPValue
sOkVal   = SAMPString (RS sampOK)
sErrVal  = SAMPString (RS sampError)
sWarnVal = SAMPString (RS sampWarning)

-- TODO: can we simplify the following by using the SAMPType instance?
instance XmlRpcType SAMPResponse where
    toValue = toValue . toSValue
    {-
    toValue (SR (Just vs) Nothing ex) = 
        toValue $ [(sStatus, sOkVal), (sResult, SAMPMap vs)] ++ ex
    toValue (SR Nothing (Just (emsg,es)) ex) =
        toValue $ [(sStatus, sErrVal), (sError, SAMPMap nvs)] ++ ex
          where
            nvs = (RS "samp.errortxt", SAMPString (RS emsg)) : es
    toValue (SR (Just vs) (Just (emsg,es)) ex) =
        toValue $ [(sStatus, sWarnVal), (sResult, SAMPMap vs),
                   (sError, SAMPMap nvs)] ++ ex
          where
            nvs = (RS "samp.errortxt", SAMPString (RS emsg)) : es
    toValue x = error $ "Invalid SAMPResponse: " ++ show x
    -}

    fromValue (ValueStruct xs) = do
        ss <- getField "samp.status" xs
        case ss of
            "samp.ok" -> do
                fs <- getField "samp.result" xs
                nxs <- mapM toSAMPKeyValue $ cleanXKeys ["samp.status", "samp.result"] xs
                return $ SR (Just fs) Nothing nxs
            "samp.error" -> do
                evals <- getField "samp.error" xs
                emsg <- getField "samp.errortxt" evals
                nes <- mapM toSAMPKeyValue $ cleanXKeys ["samp.errortxt"] evals
                nxs <- mapM toSAMPKeyValue $ cleanXKeys ["samp.status", "samp.error"] xs
                return $ SR Nothing (Just (emsg,nes)) nxs
            "samp.warning" -> do
                fs <- getField "samp.result" xs
                evals <- getField "samp.error" xs
                emsg <- getField "samp.errortxt" evals
                nes <- mapM toSAMPKeyValue $ cleanXKeys ["samp.errortxt"] evals
                nxs <- mapM toSAMPKeyValue $ cleanXKeys ["samp.status", "samp.result", "samp.error"] xs
                return $ SR (Just fs) (Just (emsg,nes)) nxs
            _ -> fail $ "Unexpected samp.status of " ++ show ss

    fromValue x = fail $ "Unable to convert to SAMP Response from " ++ show x

    getType _ = TStruct

{-
XXX TODO: add accessors so that we can avoid exposing the internals
of SAMPResponse?
-}

-- | Does the response indicate success? Note that a warning will 
-- | return `True` here; use `isSAMPWarning` for an explicit check
-- | of this case.
isSAMPSuccess :: SAMPResponse -> Bool
isSAMPSuccess (SR (Just _) _ _) = True
isSAMPSuccess _ = False

-- | Does the response indicate an error? Note that a warning will 
-- | return `True` here; use `isSAMPWarning` for an explicit check
-- | of this case.
isSAMPError :: SAMPResponse -> Bool
isSAMPError (SR _ (Just _) _) = True
isSAMPError _ = False

-- | Does the response indicate an error? Unlike `isSAMPError` this 
-- | returns `False` if the response was a warning.
isSAMPErrorOnly :: SAMPResponse -> Bool
isSAMPErrorOnly rsp = isSAMPError rsp && not (isSAMPWarning rsp)

-- | Does the response indicate a warning?
isSAMPWarning :: SAMPResponse -> Bool
isSAMPWarning (SR (Just _) (Just _) _) = True
isSAMPWarning _ = False

-- | Return the result stored in a SAMP response.
getSAMPResponseResult :: SAMPResponse -> Maybe [SAMPKeyValue]
getSAMPResponseResult (SR r _ _) = r

-- | Return the error information stored in a SAMP response.
-- | The first element of the tuple is the "samp.errortxt"
-- | value, the second element is the other values of the error map.
getSAMPResponseError :: SAMPResponse -> Maybe (RString, [SAMPKeyValue])
getSAMPResponseError (SR _ e _) = e
 
-- | Return the contents of the "samp.errortxt" return value,
-- | if provided.
getSAMPResponseErrorTxt :: SAMPResponse -> Maybe RString
getSAMPResponseErrorTxt = fmap fst . getSAMPResponseError

-- | Returns any extra information stored in the resonse beyond that
-- | specified in the profile.
getSAMPResponseExtra :: SAMPResponse -> [SAMPKeyValue]
getSAMPResponseExtra (SR _ _ ex) = ex

-- | A SAMP message, which includes the message type (`MType`),
-- | message parameters and optional key,value pairs.
data SAMPMessage = SM MType [SAMPKeyValue] [SAMPKeyValue] deriving (Eq, Show)

-- | Constructor for a `SAMPMessage`.
toSAMPMessage :: (Monad m) => 
              MType -- ^ The `MType` of the message (this is the "samp.mtype" key). It can not contain a wild card.
              -> [SAMPKeyValue]  -- ^ The parameters for the message (this is the "samp.params" key).
              -> [SAMPKeyValue]  -- ^ Any other key/values in the message.
              -> Err m SAMPMessage
toSAMPMessage mtype params extra = 
    when (isMTWildCard mtype) (throwError "MType can not contain a wild card when creating a SAMP message.")
    >> return (SM mtype params extra)

-- | What is the `MType` of the message (the "samp.mtype" key)?
getSAMPMessageType :: SAMPMessage -> MType
getSAMPMessageType (SM mt _ _) = mt

-- | What are the parameters of the message (the "samp.params" key)?
getSAMPMessageParams :: SAMPMessage -> [SAMPKeyValue]
getSAMPMessageParams (SM _ ps _) = ps

-- | What are the extra parameters of the message (may be an empty
-- | list).
getSAMPMessageExtra :: SAMPMessage -> [SAMPKeyValue]
getSAMPMessageExtra (SM _ _ ex) = ex

smtype , sparams :: RString
smtype = RS "samp.mtype"
sparams = RS "samp.params"

-- | Get a value from the contents of a SAMP Map.
getKey :: (Monad m, SAMPType a) => 
	  RString          -- ^ Field name
       -> [SAMPKeyValue]   -- ^ SAMP Map contents
       -> Err m a
getKey k xs = maybeToM ("Key " ++ show k ++ " not found")
                  (lookup k xs) >>= fromSValue

instance SAMPType SAMPMessage where
    toSValue (SM mt ps ex) = SAMPMap $ [(smtype, toSValue mt),
                                        (sparams, SAMPMap ps)]
                                       ++ ex

    fromSValue (SAMPMap xs) = do
        mt <- getKey smtype xs
        ps <- getKey sparams xs
        return $ SM mt ps (cleanKeys [smtype, sparams] xs)
    fromSValue x = throwError $ "Expected a SAMP map but sent " ++ show x

instance XmlRpcType SAMPMessage where
    toValue = toValue . toSValue

    -- can I use SAMPType for this too? At least once I have added in more
    -- instances.
    fromValue (ValueStruct xs) = do
        mt <- getField "samp.mtype" xs >>= toMTypeE
        ps <- getField "samp.params" xs >>= mapM toSAMPKeyValue
        ss <- mapM toSAMPKeyValue xs
        return $ SM mt ps (cleanKeys [smtype, sparams] ss)
    fromValue x = throwError $ "Unable to convert to SAMP Message from " ++ show x

    getType _ = TStruct

-- method calls

--
-- Types for methods calls and responses
--

{-|
A SAMP method call. Consists of a method name and a list of  
parameters.
-}
data SAMPMethodCall = SAMPMethodCall RString [SAMPValue]
     deriving (Eq, Show)

{-
XXX is it reallu necessary that the fault code be a restricted string
or not? The spec is possibly fuzzy on the details.
-}

-- | A SAMP response.
data SAMPMethodResponse =
     SAMPReturn SAMPValue -- ^ A method response returning a value
     | SAMPFault String -- ^ A fault response
     deriving (Eq, Show)

-- need to convert these to the XmlRpc equivalents

toSMC :: (Monad m) => MethodCall -> Err m SAMPMethodCall
toSMC (MethodCall n vs) = do
    ns <- maybeToM ("Unable to convert SAMP method name: " ++ n) (toRString n)
    SAMPMethodCall ns `liftM` mapM fromValue vs

fromSMC :: SAMPMethodCall -> MethodCall
fromSMC (SAMPMethodCall n vs) = MethodCall (fromRString n) (map toValue vs)

toSMR :: (Monad m) => MethodResponse -> Err m SAMPMethodResponse
toSMR (Return vs) = SAMPReturn `liftM` fromValue vs
toSMR (Fault _ msg) = return (SAMPFault msg)

fromSMR :: SAMPMethodResponse -> MethodResponse
fromSMR (SAMPReturn vs) = Return $ toValue vs
fromSMR (SAMPFault msg) = Fault 0 msg

-- | Parses a SAMP method call from the XmlRpc input.
parseSAMPCall :: (Show e, MonadError e m) =>
              String -- ^ XmlRpc input
              -> Err m SAMPMethodCall
parseSAMPCall c = parseCall c >>= toSMC

-- | Parses a SAMP method response.
parseSAMPResponse :: (Show e, MonadError e m) =>
                  String -- ^ XmlRpc input
                  -> Err m SAMPMethodResponse
parseSAMPResponse c = parseResponse c >>= toSMR

renderSAMPCall :: SAMPMethodCall -> L.ByteString
renderSAMPCall = renderCall . fromSMC

renderSAMPResponse :: SAMPMethodResponse -> L.ByteString
renderSAMPResponse = renderResponse . fromSMR 