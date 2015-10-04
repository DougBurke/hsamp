{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      :  Network.SAMP.Standard.Types
Copyright   :  (c) Douglas Burke 2011, 2013, 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr

Types for the SAMP Standard Profile modules.

Logging is provided using the @SAMP.StandardProfile.Client@
'System.Log.Logger.Logger' instance. At present this is limited
to debugging information only.

-}

module Network.SAMP.Standard.Types (

       -- * Setup

       -- | Ensure the necessary packages/systems used by the SAMP module
       -- are initialized.

       withSAMP,

       -- * Connection

       SAMPConnection(..), SAMPInfo,

       -- * SAMP Types

       SAMPType(..), SAMPValue(..), SAMPKeyValue,
       showSAMPValue,
       getKey, stringToKeyValE, stringFromKeyValE,

       RChar, toRChar, toRCharE, fromRChar,
       validRChars,

       RString, emptyRString, toRString, toRStringE, fromRString,
       asIntegral, asFloating, asBool,
       randomRString,

       MType, toMType, toMTypeE, fromMType, isMTWildCard,

       SAMPResponse, 
       toSAMPResponse, toSAMPResponseError, toSAMPResponseWarning,
       getSAMPResponseResult, getSAMPResponseError, getSAMPResponseErrorTxt,
       isSAMPSuccess, isSAMPError, isSAMPErrorOnly, isSAMPWarning,

       SAMPMessage, toSAMPMessage, getSAMPMessageType, getSAMPMessageParams,

       SAMPMethodCall(..),
       SAMPMethodResponse(..),
       parseSAMPCall, parseSAMPResponse,
       renderSAMPCall, renderSAMPResponse,

       -- * Error handling

       -- in part from haxr
       Err, runE, handleError

       ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad (liftM, ap)

import System.Random

import qualified Network.Socket as NS
import Network.XmlRpc.Internals

import Data.String
import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitOn, splitOneOf)
import Data.Char (chr, ord, isDigit, intToDigit)
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy.Char8 as L

{-|
Several of the networking libraries used by the SAMP
routines require initialization, which is provided by this
routine. This should be performed before any SAMP routine
is used (e.g. including 'Network.SAMP.Standard.Client.getHubInfoE').

At present this includes 'Network.Socket.withSocketsDo'. 
-}
withSAMP :: IO a -> IO a
withSAMP = NS.withSocketsDo
-- withSAMP = NS.withSocketsDo . NE.withHttpEnumerator

-- | Runs the SAMP computation and returns the result.
-- Any error is converted to a user exception. This is
-- a specialised form of 'handleError'.
runE :: Err IO a -> IO a
runE = handleError fail

-- | Information about the hub (the SAMP secret, the URL and
-- any other key,value entries).
type SAMPInfo = (RString, String, [(String, String)])

validRCharsAsInts :: [Int]
validRCharsAsInts = [0x09, 0x0a, 0x0d] ++ [0x20 .. 0x7f]

nRChars :: Int
nRChars = length validRCharsAsInts

-- | A list of the characters that can appear in a
-- 'RString'.
validRChars :: String
validRChars = map chr validRCharsAsInts

isRChar :: Char -> Bool
isRChar = (`elem` validRChars)

{-
isRCharAsInt :: Int -> Bool
isRCharAsInt = (`elem` validRCharsAsInts)
-}

{-|
A restricted character class that is limited to
ASCII characters with codes @0x09@, @0x0a@, @0x0d@ or 
@0x20 .. 0x7f@,
as these are the only characters supported by SAMP.

The 'Random', 'Bounded' and 'Enum' instances of @RChar@
use the full range of chacters (i.e. they include the four
control characters @0x09@, @0x0a@, @0x0d@ and @0x7f@).
-}

newtype RChar = RC Char deriving (Eq, Ord)

-- We want to display RChar/RStrings as if they were normal
-- Char/String elements

instance Show RChar where
    show (RC c) = show c
    showList = showList . map fromRChar

instance Bounded RChar where
    minBound = RC (chr 0x09)
    maxBound = RC (chr 0x7f)

-- ^ Unlike the 'Char' enumeration, we do not map from
-- 'Int' to ASCII code (and above) but use the position within
-- the 'validRChars' list as the enumerated value.
instance Enum RChar where 
    succ (RC c) | c == chr 0x7f = error "bad argument"
                | c == chr 0x09 = RC (chr 0x0a)
                | c == chr 0x0a = RC (chr 0x0d)
                | c == chr 0x0d = RC (chr 0x20)
                | otherwise     = RC (succ c)

    pred (RC c) | c == chr 0x09 = error "bad argument"
                | c == chr 0x0a = RC (chr 0x09)
                | c == chr 0x0d = RC (chr 0x0a)
                | c == chr 0x20 = RC (chr 0x0d)
                | otherwise     = RC (pred c)


    toEnum x | x < 0 || x >= nRChars = error "bad argument"
             | otherwise             = RC (validRChars !! x)

    fromEnum (RC c) = length $ takeWhile (/=c) validRChars

    enumFrom x = enumFromTo x maxBound                 

    enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound

    enumFromTo (RC s) (RC e) = toRS $ takeWhile (<=e) $ dropWhile (<s) validRChars

    -- rely on the default implementation for enumFromThenTo

-- | Create a 'RChar' from a normal 'Char'.
toRChar :: Char -> Maybe RChar
toRChar = handleError (const Nothing) . toRCharE

-- | See 'toRChar'.
toRCharE :: (Monad m) => Char -> Err m RChar
toRCharE c | isRChar c = return (RC c)
           | otherwise = throwError $ "'" ++ [c] ++ "'/" ++ toHex c ++ " is not a valid SAMP character."

-- | Extract the contents of the 'RChar'.
fromRChar :: RChar -> Char
fromRChar (RC c) = c

instance Random RChar where

    randomR (lo,hi) gen | lo > hi   = randomR (hi,lo) gen
                        | otherwise = let opts = [lo..hi]
                                          (idx, gen') = randomR (0,length opts - 1) gen
                                      in (opts !! idx, gen')

    random = randomR (minBound,maxBound)

{-|
A string that is restricted to valid SAMP characters (see 'RChar').
Note that RStrings can be empty and that there is a 'Data.String.IsString'
instance which means that the @OverloadedString@ extension can be used
when specifying @RString@ values (although note that the conversion is
unsafe since any invalid characters will result in a run-time
exception).

The type conversions between types and @RString@ values use the following BNF productions,
taken from the SAMP documentation:

>  <digit>         ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
>  <digits>        ::= <digit> | <digits> <digit>
>  <float-digits>  ::= <digits> | <digits> "." | "." <digits> | <digits> "." <digits>
>  <sign>          ::= "+" | "-"

TODO:

  - should we add a Read instance?

-}

type RString = [RChar]

{-|
The conversion provided by this instance is unsafe since 
'error' is called for those strings that contain invalid
characters.
-}
instance IsString RString where
  fromString = toRS

-- unsafe constructor

toRS :: String -> RString
toRS = fromJust . handleError error . toRStringE

instance XmlRpcType RString where
    toValue = ValueString . fromRString

    fromValue (ValueString s) = toRStringE s
    fromValue x = fail $ "Unable to convert to a SAMP string from " ++ show x

    getType _ = TString

-- | The empty string.
emptyRString :: RString
emptyRString = []

-- | Create a 'RString' from a normal 'String'.
toRString :: String -> Maybe RString
toRString = mapM toRChar

-- | See 'toRString'.
toRStringE :: (Monad m) => String -> Err m RString
toRStringE = mapM toRCharE

-- | Extract the contents of the 'RString'.
fromRString :: RString -> String
fromRString = map fromRChar

{-|
Create a random 'RString' (useful for message ids).

This is just a simple wrapper around the 'Random' instance
of 'RChar'. The argument order is chosen to make it easy to
use with 'System.Random.getStdRandom'.

Characters are chosen from the full list of SAMP characters
(e.g. 'validRChars') /except/ for @0x09@, @0x0a@, @0x0d@
and @0x7f@ (i.e. the control characters).
-}
randomRString :: (RandomGen a) 
              => Int -- ^ the number of characters (if <= 0 then an empty string is returned)
              -> a -- ^ the generator to use
              -> (RString, a)
randomRString n gen | n <= 0     = ([], gen)
                    | otherwise = go n gen []
    where
      go 0 gen' ans = (ans, gen')
      go m gen' acc = let (nc,gen'') = randomR (RC (chr 0x20), RC (chr 0x7e)) gen'
                      in go (m-1) gen'' (nc:acc)

-- helper function
rconv :: (Read a) => RString -> Maybe a
rconv [] = Nothing
rconv cs = let vs = fromRString cs
           in if all isDigit vs then Just (read vs) else Nothing

{-|
 Convert an 'RString' to an 'Integral' value using the rule:

>    <SAMP int> ::= [ <sign> ] <digits>
-}
asIntegral :: (Read a, Integral a) => RString -> Maybe a
asIntegral [] = Nothing
asIntegral s@(x:xs) | x == RC '-'  = fmap negate (rconv xs)
                    | x == RC '+'  = rconv xs
                    | otherwise    = rconv s

{-|
Convert an 'RString' to a 'Floating' value using the rule:

>    <SAMP float> ::= [ <sign> ] <float-digits> [ "e" | "E" [ <sign> ] <digits> ]
-}

asFloating :: (Read a, Floating a) => RString -> Maybe a
asFloating [] = Nothing
asFloating s@(x:xs) | x == RC '-'  = fmap negate (conv xs)
                    | x == RC '+'  = conv xs
                    | otherwise    = conv s
    where
      iconv :: RString -> Maybe Integer
      iconv = rconv

      -- this seems as if I'm complicating it a bit
      conv :: (Read a, Floating a) => RString -> Maybe a
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
                    [l2,r2] -> return (if null l2 then "0" else l2,
                                       if null r2 then "0" else r2)
                    _ -> Nothing
           e <- case es of
                  RC '+':ess -> iconv ess
                  RC '-':ess -> fmap negate (iconv ess)
                  _ -> iconv es

           return $ read (fromRString (lm ++ "." ++ rm)) ** fromIntegral e

{-
We could just check whether the string is == "0" for False,
True otherwise, but then "2.3e4" and "foo" would be treated as
True when the specification explicitly says to compare as
integer values.
-}

{-| Convert an 'RString' into a 'Bool'.

The rule is

>   <SAMP boolean> ::= "0" | "1"

although any integer value other than 0 can be used for 'True'.
-}
asBool :: RString -> Maybe Bool
asBool rs = fmap (/=0) (asIntegral rs :: Maybe Integer)

{-|
A SAMP MType. This is used to define the message
names that SAMP passes around.

They follow the following BNF productions:

>   <mchar> ::= [0-9a-z] | "-" | "_"
>   <atom>  ::= <mchar> | <atom> <mchar>
>   <mtype> ::= <atom> | <mtype> "." <atom>

We also allow wildcards in certain cases, which is

>  <msub>  ::= "*" | <mtype> "." "*"

The 'Eq' instance for @MType@ supports wild cards in
equality tests, so that

>  toMType "image.*" == toMType "image.load.fits"

is 'True'.

TODO:

 - do we want a Read instance?

-}

data MType = MT
     String -- if a wildcard then drop the * but NOT the last . (unless "*")
     Bool -- is this a wildcard

instance Show MType where
    show (MT mt False) = mt
    show (MT mt True)  = mt ++ "*"

{-|
The conversion provided by this instance is unsafe since 
'error' is called for those strings that contain invalid
characters.
-}
instance IsString MType where
    fromString = fromJust . handleError error . toMTypeE

{-|
Returns 'True' if the two mtypes are equal. If neither
include a wild card then the check is a simple compare.
If one has a wild card then the comparison is defined
by the wildcard, so that 'table.load.*' matches 'table.load.votable'.
For two wildcards we match on the MType fragments,
so that 'table.*' matches 'table.load.*'.
-}
matchMTypes :: MType -> MType -> Bool
matchMTypes (MT l False) (MT r False) = l == r
matchMTypes (MT l True)  (MT r False) = l `isPrefixOf` r
matchMTypes (MT l False) (MT r True)  = r `isPrefixOf` l
matchMTypes (MT l True)  (MT r True)  = l `isPrefixOf` r || r `isPrefixOf` l

instance Eq MType where
    (==) = matchMTypes

instance SAMPType MType where
    toSValue = fromString . show

    fromSValue (SAMPString s) = toMTypeE $ fromRString s
    fromSValue x = throwError $ "Expected a string, sent " ++ show x

instance XmlRpcType MType where
    toValue = toValue . show

    fromValue (ValueString s) = toMTypeE s
    fromValue x = throwError $ "Unable to convert to a SAMP MType from " ++ show x

    getType _ = TString

mtchars :: String
mtchars = ['0' .. '9'] ++ ['a' .. 'z'] ++ "-_."

isMTChar :: Char -> Bool
isMTChar = (`elem` mtchars)

-- | See 'toMTypeE'.
toMType :: String -> Maybe MType
toMType = handleError (return Nothing) . toMTypeE

{-|
Create a 'MType'. This includes wild cards such as
@*@ or @file.event.*@.
-}
toMTypeE :: (Monad m) => String -> Err m MType
toMTypeE mt = go mt ""
     where
       hdr = "Invalid MType '" ++ mt ++ "'"

       -- really should use a simple parser/fsa
       -- nb: we need to store the last '.' in a wildcard
       --     for the Eq instance
       go "" acc = return $ MT (reverse acc) False
       go ".*" acc | null acc  = throwError $ hdr ++ " (no characters before '.')"
                   | otherwise = return $ MT (reverse ('.':acc)) True
       go "*" acc | null acc  = return $ MT "" True
                  | otherwise = throwError $ hdr ++ " (missing '.' before wildcard)"

       go ('*':_) _ = throwError $ hdr ++ " (wildcard can only appear at end)"
       go "." _ = throwError $ hdr ++ " (missing name or wildcard after '.')"
       go ('.':'.':_) _ = throwError $ hdr ++ " (there must be characters between the '.')"

       go (x:xs) acc | isMTChar x = go xs (x:acc)
                     | otherwise  = throwError $ hdr ++ " (invalid character '" ++ [x] ++ "'/" ++ toHex x ++ ")" -- could include hex code

toHex :: Char -> String
toHex c = let x = ord c
              y = [x `div` 16, x `mod` 16]
          in "0x" ++ map intToDigit y

-- | Extract the contents of the 'MType'.
fromMType :: MType -> String
fromMType = show

-- | Does the 'MType' contain a wild card?
isMTWildCard :: MType -> Bool
isMTWildCard (MT _ f) = f

-- | This represents a key,value pair stored in a SAMP map.
-- Note that the key is stored as a 'RString'.
type SAMPKeyValue = (RString, SAMPValue)

{-
TODO: should there be a SAMPType instance of (MType, SAMPValue)
for declareSubscriptionsE in Client?
-}

-- We can not add a Show instance of SAMPKeyValue without
-- getting a lot of complaints from deriving instances

{-|
Convert a pair of strings into a 'SAMPKeyValue'.
The routine fails if either of the input strings can not be
converted into 'RString' values.

If the @OverloadedStrings@ GHC extension is set then you can
just use the 'Data.String.IsString' instances for 'RString'
and 'SAMPValue' to say

>    let kv = ("author.name", "foo@bar.com")

rather than

>    kv <- stringToKeyValE "author.name" "foo@bar.com"

-}
stringToKeyValE :: (Monad m, Functor m) -- added Functor instance as inferred by ghc
                => String -- ^ the key name
                -> String -- ^ the value
                -> Err m SAMPKeyValue
stringToKeyValE k v = do
    km <- toRStringE k
    vm <- toRStringE v
    return (km, toSValue vm)

-- stringToKeyValE k v = (,) `liftM` toRStringE k `ap` fmap toSValue (toRStringE v)


{-
TODO
  - document
  - work out fixity
  - decide if this is actualy sensible

(|>) :: (Monad m)
     => String
     -> String
     -> Err m SAMPKeyValue
(|>) = stringToKeyValE

and (<|) could be for conversion back to strings?
-}

-- | Convert a 'SAMPKeyValue' into a pair of strings.
-- This fails if the 'SAMPValue' stored in the pair is not
-- a 'SAMPString'.
stringFromKeyValE :: Monad m
                 => SAMPKeyValue
                 -> Err m (String, String)
stringFromKeyValE (k,SAMPString v) = return (fromRString k, fromRString v)
stringFromKeyValE (k,x) = throwError $ "Key " ++ show k ++ " should be a SAMP string but found " ++ show x

{-
The following routines could be made generic - ie use XmlRpcType a rather
than force Value, but need to look at to see if it is worth it.
-}

-- | Convert a (String, Value) tuple into (RString, SAMPValue)
toSAMPKeyValue :: (Monad m) => (String, Value) -> Err m SAMPKeyValue
toSAMPKeyValue (n,v) = (,) `liftM` toRStringE n `ap` fromValue v

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

Strings are restricted to the 'RString' class; they may
be used to serialize numeric types, as defined by the
message (ie 'MType'). There are several routines for
converting a 'RString' to a numeric type following the
syntax given in the SAMP recommendation.

For SAMP maps we often just pass around @[SAMPKeyValue]@ rather
than the @SAMPValue@ container.
-}

data SAMPValue =
    SAMPString RString
  | SAMPList [SAMPValue]
  | SAMPMap [SAMPKeyValue]
  deriving (Eq, Show)

{-|
The conversion provided by this instance is unsafe since 
'error' is called for those strings that contain invalid
characters.
-}
instance IsString SAMPValue where
  fromString = SAMPString . toRS

instance XmlRpcType SAMPValue where
    toValue (SAMPString s) = toValue s
    toValue (SAMPList xs) = ValueArray $ map toValue xs
    toValue (SAMPMap xs) = ValueStruct [(fromRString n, toValue v) | (n,v) <- xs]

    fromValue (ValueString s) = liftM SAMPString $ toRStringE s
    fromValue (ValueArray xs) = liftM SAMPList $ mapM fromValue xs
    fromValue (ValueStruct xs) = liftM SAMPMap $ mapM toSAMPKeyValue xs
    
    -- convert as strings
    fromValue (ValueUnwrapped s) = liftM SAMPString $ toRStringE s
    
    fromValue x = fail $ "Unable to convert to SAMP Value from " ++ show x

    getType (SAMPString _) = TString
    getType (SAMPList _) = TArray
    getType (SAMPMap _) = TStruct

-- TODO: improve this

-- | Convert a 'SAMPValue' to a displayable string. This is intended for
-- debugging and simple screen output rather than serialisation.
showSAMPValue :: SAMPValue -> String
showSAMPValue (SAMPString s) = show s
showSAMPValue (SAMPList xs) = concat ["[", intercalate "," (map showSAMPValue xs), "]"]
showSAMPValue (SAMPMap ms) = concat ["{", intercalate "," vals, "}"]
         where
             vals = map (\(n,v) -> concat [show n, " -> ", showSAMPValue v]) ms

-- | Conversion routines for 'SAMPValue' values. This is intended to
-- make it easier to convert between Haskell and SAMP types.
class SAMPType a where
    -- | Convert to a SAMP type
    toSValue :: a -> SAMPValue

    -- | convert from a SAMP type
    fromSValue :: (Monad m) => SAMPValue -> Err m a

instance {-# OVERLAPPING #-} SAMPType RString where
    toSValue = SAMPString
    fromSValue (SAMPString s) = return s
    fromSValue x = throwError $ "Expected a string, sent " ++ show x

instance SAMPType Bool where
    toSValue True = "1"
    toSValue _    = "0"

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to a Bool.") (asBool s)
    fromSValue x = throwError $ "Expected a string but sent " ++ show x

instance SAMPType Int where
    toSValue = fromString . show

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to an Int.") (asIntegral s)
    fromSValue x = throwError $ "Expected a string but sent " ++ show x

instance SAMPType Integer where
    toSValue = fromString . show

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to an Integer.") (asIntegral s)
    fromSValue x = throwError $ "Expected a string but sent " ++ show x

instance {-# OVERLAPPING #-} SAMPType [SAMPKeyValue] where
    toSValue = SAMPMap
    fromSValue (SAMPMap xs) = return xs
    fromSValue x = throwError $ "Expected a SAMP map, sent " ++ show x

instance {-# OVERLAPPABLE #-} (SAMPType a) => SAMPType [a] where
    toSValue = SAMPList . map toSValue
    fromSValue (SAMPList xs) = mapM fromSValue xs
    fromSValue x = throwError $ "Expected a SAMP list, sent " ++ show x

{-
TODO: need to encode a float using the correct rules.

instance SAMPType Float where
    toSValue = fromString . show

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to an Integer.") (asIntegral s)
    fromSValue x = throwError $ "Expected a string but sent " ++ show x
-}

-- How can we make it so that this data is invalid once we unregister
-- a client? Any calls with a previously valid structure will fail,
-- unless there's some fancy-schmancy type class magic to avoid it,
-- which I'm sure there is; e.g. a phantom type or the use of
-- monadic regions.
--

{-|
This record stores the information needed by a client to talk to a hub.
It is created by

  - reading the SAMP hub location

  - registering the client with the hub

which should be done by routines from the "Network.SAMP.Standard.Client"
module (e.g. 'Network.SAMP.Standard.Client.registerClientE').
-}
data SAMPConnection = SAMPConnection {
     scSecret :: RString, -- ^ the SAMP secret, from the hub file
     scHubURL :: String, -- ^ the URL of the server
     scPrivateKey :: RString, -- ^ the private key assigned to the client by the hub
     scHubId :: RString, -- ^ the name of the hub
     scId :: RString -- ^ the name of the client (assigned by the hub)
     } deriving (Eq, Show)

{-|
The response from a client to a SAMP call.

Use the 'isSAMPSuccess', 'isSAMPError' and 'isSAMPWarning' routines to
query the status of the response. The 'toSAMPResponse', 'toSAMPResponseError'
and 'toSAMPResponseWarning' routines are used to create @SAMPResponse@
values.

-}
data SAMPResponse =
    SR (Maybe [SAMPKeyValue]) (Maybe (RString, [SAMPKeyValue]))
    deriving (Eq, Show)

instance SAMPType SAMPResponse where
    toSValue (SR (Just vs) Nothing) = 
        SAMPMap [(sStatus, sOkVal), (sResult, SAMPMap vs)]
    toSValue (SR Nothing (Just (emsg,es))) =
        SAMPMap [(sStatus, sErrVal), (sError, SAMPMap nvs)]
          where
            nvs = (sErrorTxt, SAMPString emsg) : es
    toSValue (SR (Just vs) (Just (emsg,es))) =
        SAMPMap [(sStatus, sWarnVal), (sResult, SAMPMap vs),
                 (sError, SAMPMap nvs)]
          where
            nvs = (sErrorTxt, SAMPString emsg) : es
    toSValue x = error $ "Invalid SAMPResponse: " ++ show x

    fromSValue (SAMPMap xs) = do
        ss <- liftM fromRString $ getKey sStatus xs
        let getError = do
              evals <- getKey sError xs
              emsg <- getKey sErrorTxt evals
              let nes = cleanKeys [sErrorTxt] evals
              return (Just (emsg,nes))
              
        case ss of
          "samp.ok" -> do
            fs <- getKey sResult xs
            return $ SR (Just fs) Nothing

          "samp.error" -> do
            err <- getError  
            return $ SR Nothing err

          "samp.warning" -> do
            fs <- getKey sResult xs
            err <- getError
            return $ SR (Just fs) err

          _ -> throwError $ "Unexpected samp.status of " ++ show ss

    fromSValue x = throwError $ "Expected a SAMP map but sent " ++ show x

-- | Create a SAMP response that indicates success
toSAMPResponse :: [SAMPKeyValue] -- ^ key,value pairs to reply to the caller
               -> SAMPResponse
toSAMPResponse svals = SR (Just svals) Nothing

-- | Create a SAMP response that indicates an error
toSAMPResponseError :: RString -- ^ the error test (@samp.errortxt@)
                    -> [SAMPKeyValue] -- ^ other elements of the error
                    -> SAMPResponse
toSAMPResponseError emsg evals = SR Nothing (Just (emsg,evals))

-- | Create a SAMP response that indicates a warning.
toSAMPResponseWarning :: [SAMPKeyValue] -- ^ successful key,value pairs
                      -> RString -- ^ error message (@samp.errortxt@)
                      -> [SAMPKeyValue] -- ^ other elements of the error
                      -> SAMPResponse
toSAMPResponseWarning svals emsg evals = SR (Just svals) (Just (emsg,evals))

sStatus , sResult , sError , sErrorTxt :: RString
sStatus = "samp.status"
sResult = "samp.result"
sError  = "samp.error"
sErrorTxt = "samp.errortxt"

sOkVal , sErrVal , sWarnVal :: SAMPValue
sOkVal   = "samp.ok"
sErrVal  = "samp.error"
sWarnVal = "samp.warning"

instance XmlRpcType SAMPResponse where
    toValue = toValue . toSValue

    -- TODO: can we simplify the following by using the SAMPType instance?
    fromValue (ValueStruct xs) = do
        ss <- getField "samp.status" xs :: (Monad m) => Err m RString
        let getError = do
              evals <- getField "samp.error" xs
              emsg <- getField "samp.errortxt" evals
              nes <- mapM toSAMPKeyValue $ cleanXKeys ["samp.errortxt"] evals
              return $ Just (emsg, nes)
              
        case ss of
          "samp.ok" -> do
            fs <- getField "samp.result" xs
            return $ SR (Just fs) Nothing
          "samp.error" -> do
            err <- getError
            return $ SR Nothing err
          "samp.warning" -> do
            fs <- getField "samp.result" xs
            err <- getError
            return $ SR (Just fs) err
          _ -> fail $ "Unexpected samp.status of " ++ show ss

    fromValue x = fail $ "Unable to convert to SAMP Response from " ++ show x

    getType _ = TStruct

{-|
Does the response indicate success? Note that a warning will 
return 'True' here; use 'isSAMPWarning' for an explicit check
of this case.
-}
isSAMPSuccess :: SAMPResponse -> Bool
isSAMPSuccess (SR (Just _) _) = True
isSAMPSuccess _ = False

{-|
Does the response indicate an error? Note that a warning will 
return 'True' here; use 'isSAMPWarning' for an explicit check
of this case.
-}
isSAMPError :: SAMPResponse -> Bool
isSAMPError (SR _ (Just _)) = True
isSAMPError _ = False

{-|
Does the response indicate an error? Unlike 'isSAMPError' this 
returns 'False' if the response was a warning.
-}
isSAMPErrorOnly :: SAMPResponse -> Bool
isSAMPErrorOnly rsp = isSAMPError rsp && not (isSAMPWarning rsp)

-- | Does the response indicate a warning?
isSAMPWarning :: SAMPResponse -> Bool
isSAMPWarning (SR (Just _) (Just _)) = True
isSAMPWarning _ = False

-- | Return the result stored in a SAMP response.
getSAMPResponseResult :: SAMPResponse -> Maybe [SAMPKeyValue]
getSAMPResponseResult (SR r _) = r

{-|
Return the error information stored in a SAMP response.
The first element of the tuple is the @samp.errortxt@
value, the second element is the other values of the error map.
-}
getSAMPResponseError :: SAMPResponse -> Maybe (RString, [SAMPKeyValue])
getSAMPResponseError (SR _ e) = e

{-|
Return the contents of the @samp.errortxt@ return value,
if provided.
-}
getSAMPResponseErrorTxt :: SAMPResponse -> Maybe RString
getSAMPResponseErrorTxt = fmap fst . getSAMPResponseError

{-|
A SAMP message, which contains the message type ('MType')
and the message parameters as a list of key,value pairs.
-}
data SAMPMessage = SM MType [SAMPKeyValue] deriving (Eq, Show)

{-|
Constructor for a 'SAMPMessage'.

-}
toSAMPMessage :: (Monad m) => 
              MType -- ^ The 'MType' of the message (this is the @samp.mtype@ key). It can not contain a wild card.
              -> [SAMPKeyValue]  -- ^ The parameters for the message (this is the @samp.params@ key).
              -> Err m SAMPMessage
toSAMPMessage mtype params
    | isMTWildCard mtype = throwError "MType can not contain a wild card when creating a SAMP message."
    | otherwise          = return (SM mtype params)

-- | What is the 'MType' of the message (the @samp.mtype@ key)?
getSAMPMessageType :: SAMPMessage -> MType
getSAMPMessageType (SM mt _) = mt

-- | What are the parameters of the message (the @samp.params@ key)?
getSAMPMessageParams :: SAMPMessage -> [SAMPKeyValue]
getSAMPMessageParams (SM _ ps) = ps

smtype , sparams :: RString
smtype = toRS "samp.mtype"
sparams = toRS "samp.params"

-- | Get a value from the contents of a SAMP Map (given as a list
-- of key,value pairs).
getKey :: (Monad m, SAMPType a) => 
       RString             -- ^ Field name
       -> [SAMPKeyValue]   -- ^ SAMP Map contents
       -> Err m a
getKey k xs = maybeToM ("Key " ++ show k ++ " not found")
                  (lookup k xs) >>= fromSValue

instance SAMPType SAMPMessage where
    toSValue (SM mt ps) = SAMPMap [(smtype, toSValue mt),
                                   (sparams, SAMPMap ps)]

    fromSValue (SAMPMap xs) = do
        mt <- getKey smtype xs
        ps <- getKey sparams xs
        return $ SM mt ps
    fromSValue x = throwError $ "Expected a SAMP map but sent " ++ show x

instance XmlRpcType SAMPMessage where
    toValue = toValue . toSValue

    -- can I use SAMPType for this too? At least once I have added in more
    -- instances.
    fromValue (ValueStruct xs) = do
        mt <- getField "samp.mtype" xs >>= toMTypeE
        ps <- getField "samp.params" xs >>= mapM toSAMPKeyValue
        return $ SM mt ps
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
XXX is it really necessary that the fault code be a restricted string
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