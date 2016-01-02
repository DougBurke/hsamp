{-# LANGUAGE DeriveGeneric #-}
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
       toSAMPInfo,
       getSAMPInfoLocation,
       getSAMPInfoHubURL,
       getSAMPInfoHubSecret,
       getSAMPInfoHubMetadata,

       -- * SAMP Types

       SAMPType(..), SAMPValue(..), SAMPMapValue, SAMPMapElement,
       showSAMPValue,
       stringToMapElementE, stringFromMapElementE,

       RChar, toRChar, toRCharE, fromRChar,
       validRChars,

       RString, emptyRString, toRString, toRStringE, fromRString,
       asIntegral, asFloating, asBool,
       randomRString, randomAlphaNumRString,

       MType, toMType, toMTypeE, fromMType, fromMTypeRS, isMTWildCard,

       MessageTag, toMessageTag, fromMessageTag,

       MessageId, toMessageId, fromMessageId,

       ClientName, toClientName, fromClientName,

       HubSecret, toHubSecret, fromHubSecret,
       ClientSecret, toClientSecret, fromClientSecret,
                
       SAMPResponse, 
       toSAMPResponse, toSAMPResponseError, toSAMPResponseWarning,
       getSAMPResponseResult, getSAMPResponseError, getSAMPResponseErrorTxt,
       getSAMPResponseExtra,
       isSAMPSuccess, isSAMPError, isSAMPErrorOnly, isSAMPWarning,

       SAMPMessage,
       toSAMPMessage, toSAMPMessageMT,
       getSAMPMessageType, getSAMPMessageParams, getSAMPMessageExtra,
                          
       SAMPMethodCall(..),
       SAMPMethodResponse(..),
       parseSAMPCall, parseSAMPResponse,
       renderSAMPCall, renderSAMPResponse,

       getKey,
       removeKeyE,
       cleanKeys,

       dumpSAMPValue,
       
       -- * Error handling

       -- in part from haxr
       Err, runE, handleError

       ) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
    
import qualified Network.Socket as NS

import Control.Monad.Except (MonadError, throwError)
import Control.Monad (liftM, ap)

import Data.Char (chr, isAlphaNum, isDigit, ord)
import Data.Hashable (Hashable)
import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (fromJust)
import Data.String (IsString(..))

import GHC.Generics (Generic)

import Network.URI (URI)
import Network.XmlRpc.Internals (Err, XmlRpcType(..), Value(..), Type(..),
                                 MethodCall(..), MethodResponse(..),
                                 renderCall, renderResponse,
                                 parseCall, parseResponse,
                                 toValue, handleError)
import Numeric (showHex)                                 
    
import System.Random (Random(random), RandomGen, randomR)

{-|
Several of the networking libraries used by the SAMP
routines require initialization, which is provided by this
routine. This should be performed before any SAMP routine
is used (e.g. including 'Network.SAMP.Standard.Setup.getHubInfoE').

At present this includes 'Network.Socket.withSocketsDo'. 
-}
withSAMP :: IO a -> IO a
withSAMP = NS.withSocketsDo
-- withSAMP = NS.withSocketsDo . NE.withHttpEnumerator

-- | My version of haxr's maybeToM. It is specialized to Err, but
--   most importantly, uses throwError rather than fail.
maybeToM :: Monad m => String -> Maybe a -> Err m a
maybeToM _ (Just x) = return x
maybeToM err Nothing = throwError err

-- | Runs the SAMP computation and returns the result.
-- Any error is converted to a user exception. This is
-- a specialised form of 'handleError'.
runE :: Err IO a -> IO a
runE = handleError fail

-- | Information about the hub.
data SAMPInfo = SI {
      _siLocation :: URI
    , _siSecret :: HubSecret
    , _siHubURL :: String
    , _siMetadata :: M.Map String String -- Should these be RString?
    }

toSAMPInfo ::
    URI        -- ^ location of the hub
    -> HubSecret -- ^ the secret for the hub
    -> String    -- ^ the URL of the hub
    -> M.Map String String  -- ^ extra metadata
    -> SAMPInfo
toSAMPInfo loc secret url metadata =
    SI { _siLocation = loc
       , _siSecret = secret
       , _siHubURL = url
       , _siMetadata = metadata
       }

getSAMPInfoLocation :: SAMPInfo -> URI
getSAMPInfoLocation = _siLocation
    
getSAMPInfoHubURL :: SAMPInfo -> String
getSAMPInfoHubURL = _siHubURL
    
getSAMPInfoHubSecret :: SAMPInfo -> HubSecret
getSAMPInfoHubSecret = _siSecret
    
getSAMPInfoHubMetadata :: SAMPInfo -> M.Map String String
getSAMPInfoHubMetadata = _siMetadata
    
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

newtype RChar = RC { _unRC :: Char } deriving (Eq, Ord, Generic)

instance Hashable RChar
  
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

    fromEnum (RC c) = length (takeWhile (/=c) validRChars)

    enumFrom x = enumFromTo x maxBound                 

    enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound

    enumFromTo (RC s) (RC e) = toRS (takeWhile (<=e)
                                     (dropWhile (<s) validRChars))

    -- rely on the default implementation for enumFromThenTo

-- | Create a 'RChar' from a normal 'Char'.
toRChar :: Char -> Maybe RChar
toRChar = handleError (const Nothing) . toRCharE

-- | See 'toRChar'.
toRCharE :: (Monad m) => Char -> Err m RChar
toRCharE c | isRChar c = return (RC c)
           | otherwise = throwError ("'" ++ [c] ++ "'/" ++ toHex c ++
                                     " is not a valid SAMP character.")

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
The conversion provided by this instance is unsafe:
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
    fromValue x = throwError ("Unable to convert to a SAMP string from " ++
                              show x)

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
randomRString ::
    (RandomGen g) 
    => Int
    -- ^ the number of characters (if <= 0 then an empty string is returned)
    -> g -- ^ the generator to use
    -> (RString, g)
randomRString n gen | n <= 0     = ([], gen)
                    | otherwise = go n gen []
    where
      go 0 gen' ans = (ans, gen')
      go m gen' acc = let (nc,gen'') = randomR (RC (chr 0x20), RC (chr 0x7e)) gen'
                      in go (m-1) gen'' (nc:acc)

-- | Similar to `randomRString`, but restrict the characters that
--   are chosen to the alpha-numeric set (as reported by
--   Data.Char.isAlphaNum).
--
randomAlphaNumRString ::
    (RandomGen g) 
    => Int
    -- ^ the number of characters (if <= 0 then an empty string is returned)
    -> g -- ^ the generator to use
    -> (RString, g)
randomAlphaNumRString n gen | n <= 0     = ([], gen)
                    | otherwise = go n gen []
    where
      go 0 gen' ans = (ans, gen')
      -- throw out invalid characters (since the alpha-num characters
      -- aren't in a consecutive range)
      go m gen' acc = let (nc,gen'') = randomR (RC (chr 0x20), RC (chr 0x7e)) gen'
                      in if isAlphaNum (_unRC nc)
                         then go (m-1) gen'' (nc:acc)
                         else go m gen'' acc

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

>   <mchar> ::= [0-9A-Za-z] | "-" | "_"
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

 - do we need to add some way of indicating 'x-samp' values
   and supporting approximate-equality in checks?

 - perhaps there should be two versions: a type for the
   message itself (so it has to be complete, no wildcards)
   and one for the general case, which can include wildcards?
-}

data MType = MT
     String -- if a wildcard then drop the * but NOT the last . (unless "*")
     Bool -- is this a wildcard
     deriving Generic

instance Hashable MType
    
{-
-- standalone deriving is used here to add the comment about the Ord
-- instance

-- | The Ord instance is so that MTypes can be used as keys in
--   a map. There is currently no semantics behind this ordering
--   (i.e. is "samp.hub.*" greater or less than "samp.hub").
--
--   This striked me as a dangerous thing to have given that the
--   Eq instance handles wild cards; really need two different
--   types and a container type
--      MType "actual"
--      MType "wildcard"
--      MType (Either "actual" "wildcard")
--   and give appropriate instances
--
deriving instance Ord MType
-}

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

This does *not* consider 'samp.foo' to be the same as
'x-samp.foo'.
-}
matchMTypes :: MType -> MType -> Bool
matchMTypes (MT l False) (MT r False) = l == r
matchMTypes (MT l True)  (MT r False) = l `isPrefixOf` r
matchMTypes (MT l False) (MT r True)  = r `isPrefixOf` l
matchMTypes (MT l True)  (MT r True)  = l `isPrefixOf` r || r `isPrefixOf` l

-- | The equality check includes comparison for wild cards
--   (so that 'samp.hub.register' and 'samp.*' are considered
--   equal). This may prove to be an unwise choice (e.g. if
--   used as an index in a data structure).
--
instance Eq MType where
    (==) = matchMTypes

instance SAMPType MType where
    toSValue = fromString . show

    fromSValue (SAMPString s) = toMTypeE (fromRString s)
    fromSValue x = throwError ("Expected a string, sent " ++ show x)

instance XmlRpcType MType where
    toValue = toValue . show

    fromValue (ValueString s) = toMTypeE s
    fromValue x = throwError ("Unable to convert to a SAMP MType from " ++
                              show x)

    getType _ = TString

mtchars :: String
mtchars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "-_."

-- would likely be faster to do a numeric check
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

       dotFirst = hdr ++ " (no characters before '.')"
       missingDot = hdr ++ " (missing '.' before wildcard)"
       invalidWild = hdr ++ " (wildcard can only appear at end)"
       missingWild = hdr ++ " (missing name or wildcard after '.')"
       multiDots = hdr ++ " (there must be characters between the '.')"

       invalidChar x = hdr ++ " (invalid character '" ++ [x] ++ "'/" ++
                       toHex x ++ ")"
                    
       -- really should use a simple parser/fsa
       -- nb: we need to store the last '.' in a wildcard
       --     for the Eq instance
       go "" acc = return $ MT (reverse acc) False
       go ".*" acc | null acc  = throwError dotFirst
                   | otherwise = return (MT (reverse ('.':acc)) True)
       go "*" acc | null acc  = return (MT "" True)
                  | otherwise = throwError missingDot

       go ('*':_) _ = throwError invalidWild
       go "." _ = throwError missingWild
       go ('.':'.':_) _ = throwError multiDots

       go (x:xs) acc | isMTChar x = go xs (x:acc)
                     | otherwise  = throwError (invalidChar x)

-- | Convert a character to a hexadecimal encoding, ensuring 0-padded
--   (i.e. the length is an even number).
--
--   It could act like ShowS but avoid that for now.
--
toHex :: Char -> String
toHex c =
    let s = showHex (ord c) ""
        n = length s `mod` 2
    in "0x" ++ replicate n '0' ++ s

-- | Extract the contents of the 'MType'.
fromMType :: MType -> String
fromMType = show

-- | Extract the contents of the 'MType'.
fromMTypeRS :: MType -> RString
fromMTypeRS = fromJust . toRString . show  -- a valid MType is a valid RString

-- | Does the 'MType' contain a wild card?
isMTWildCard :: MType -> Bool
isMTWildCard (MT _ f) = f

type SAMPMapElement = (RString, SAMPValue)
type SAMPMapValue = M.Map RString SAMPValue

{-
TODO: should there be a SAMPType instance of (MType, SAMPValue)
for declareSubscriptionsE in Client?
-}

{-|
Convert a pair of strings into a 'SAMPMapElement'.

The routine fails if either of the input strings can not be
converted into 'RString' values.

If the @OverloadedStrings@ GHC extension is set then you can
just use the 'Data.String.IsString' instances for 'RString'
and 'SAMPValue' to say

>    let kv = ("author.name", "foo@bar.com")

rather than

>    kv <- stringToMapElementE "author.name" "foo@bar.com"

-}

stringToMapElementE ::
    (Monad m)
    => String -- ^ the key name
    -> String -- ^ the value
    -> Err m SAMPMapElement
stringToMapElementE k v = do
    km <- toRStringE k
    vm <- toRStringE v
    return (km, toSValue vm)


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

-- | Convert a 'SAMPMapElement' into a pair of strings.
-- This fails if the 'SAMPValue' stored in the pair is not
-- a 'SAMPString'.

stringFromMapElementE ::
    (Monad m)
    => SAMPMapElement
    -> Err m (String, String)
stringFromMapElementE (k, SAMPString v) =
  return (fromRString k, fromRString v)
stringFromMapElementE (k, x) =
  throwError ("Key " ++ show k ++ " should be a SAMP string but found " ++
              show x)

{-
The following routines could be made generic - ie use XmlRpcType a rather
than force Value, but need to look at to see if it is worth it.
-}

-- | Convert a (String, Value) tuple into (RString, SAMPValue)
toSAMPMapElement ::
    (Monad m)
    => (String, Value)
    -> Err m SAMPMapElement
toSAMPMapElement (n,v) = (,) `liftM` toRStringE n `ap` fromValue v

              
-- | Get a value from the contents of a SAMP Map (given as a list
-- of key,value pairs).
getKey ::
    (Monad m, SAMPType a)
    => RString          -- ^ Field name
    -> SAMPMapValue     -- ^ SAMP Map
    -> Err m a
getKey k xs = maybeToM ("Key " ++ show k ++ " not found")
                  (M.lookup k xs) >>= fromSValue

-- | Remove the listed keys (they do not have to exist).
cleanKeys :: [RString] -> SAMPMapValue -> SAMPMapValue
cleanKeys ks = M.filterWithKey remove
  where
    remove k _ = k `notElem` ks

-- | Extract the value of the key from the map and a new map
--   where the key has been removed.
removeKeyE ::
  (Ord a, Show a, Monad m)
  => a   -- ^ key to find
  -> M.Map a b -- ^ map to search
  -> Err m (b, M.Map a b)
  -- ^ value of the key and the new map (without this key)
  --   otherwise errors out if the key does not exist.
removeKeyE k old =
  let (mval, new) = M.updateLookupWithKey remove k old
      remove _ _ = Nothing
  in case mval of
    Just val -> return (val, new)
    Nothing -> throwError ("Unable to find key: " ++ show k)

{-
removeKeyE ::
  Monad m
  => RString   -- ^ key to find
  -> SAMPMapValue -- ^ map to search
  -> Err m (SAMPValue, SAMPMapValue)
  -- ^ value of the key and the new map (without this key)
  --   otherwise errors out if the key does not exist.

grabKeyE ::
    (Monad m, SAMPType a)
    => RString -- ^ key to find
    -> SAMPMapValue
    -> Err m (a, SAMPMapValue) -- ^ value of key and remainder of the map
-}
    
grabKeyE ::
    (Ord a, Show a, Monad m, SAMPType b)
    => a -- ^ key to find
    -> M.Map a SAMPValue
    -> Err m (b, M.Map a SAMPValue) -- ^ value of key and remainder of the map
grabKeyE k kvs = do
    (sval, rs) <- removeKeyE k kvs
    val <- fromSValue sval
    return (val, rs)

      
{- | The SAMP profile supports three basic data types,
a string, a list or a map (keys are strings and they
can contain a SAMP data type).

Strings are restricted to the 'RString' class; they may
be used to serialize numeric types, as defined by the
message (ie 'MType'). There are several routines for
converting a 'RString' to a numeric type following the
syntax given in the SAMP recommendation.

-}

data SAMPValue =
    SAMPString RString
  | SAMPList [SAMPValue]
  | SAMPMap SAMPMapValue
  deriving (Show, Eq)

{-|
The conversion provided by this instance is unsafe since 
'error' is called for those strings that contain invalid
characters.
-}
instance IsString SAMPValue where
  fromString = SAMPString . toRS

instance XmlRpcType SAMPValue where
    toValue (SAMPString s) = toValue s
    toValue (SAMPList xs) = ValueArray (map toValue xs)
    toValue (SAMPMap xs) = ValueStruct [(fromRString n, toValue v) |
                                        (n,v) <- M.toList xs]

    fromValue (ValueString s) = liftM SAMPString (toRStringE s)
    fromValue (ValueArray xs) = liftM SAMPList (mapM fromValue xs)
    fromValue (ValueStruct xs) =
      liftM (SAMPMap . M.fromList) (mapM toSAMPMapElement xs)
    
    -- convert as strings
    fromValue (ValueUnwrapped s) = liftM SAMPString (toRStringE s)
    
    fromValue x = throwError ("Unable to convert to SAMP Value from " ++
                              show x)

    getType (SAMPString _) = TString
    getType (SAMPList _) = TArray
    getType (SAMPMap _) = TStruct

instance J.ToJSON SAMPValue where
    toJSON (SAMPString s) = J.String (T.pack (fromRString s))
    toJSON (SAMPList xs)  = J.toJSON (map J.toJSON xs)
    toJSON (SAMPMap ms)  = J.Object (HM.fromList (map conv kvs))
        where
          kvs = M.toList ms
          conv (k, vs) = (T.pack (fromRString k), J.toJSON vs)
                            
-- TODO: improve this

-- | Convert a 'SAMPValue' to a displayable string. This is intended for
-- debugging and simple screen output rather than serialisation.
showSAMPValue :: SAMPValue -> String
showSAMPValue (SAMPString s) = show s
showSAMPValue (SAMPList xs) =
    concat ["[", intercalate "," (map showSAMPValue xs), "]"]
showSAMPValue (SAMPMap ms) =
    concat ["{", intercalate "," vals, "}"]
        where
          kvs = M.toList ms
          vals = map (\(n,v) -> concat [show n, " -> ", showSAMPValue v]) kvs

-- | Conversion routines for 'SAMPValue' values. This is intended to
-- make it easier to convert between Haskell and SAMP types.
class SAMPType a where
    -- | Convert to a SAMP type
    toSValue :: a -> SAMPValue

    -- | convert from a SAMP type
    fromSValue :: (Monad m) => SAMPValue -> Err m a

instance SAMPType SAMPValue where
    toSValue = id
    fromSValue = return
                  
instance {-# OVERLAPPING #-} SAMPType RString where
    toSValue = SAMPString
    fromSValue (SAMPString s) = return s
    fromSValue x = throwError ("Expected a string but sent " ++ show x)

instance SAMPType Bool where
    toSValue True = "1"
    toSValue _    = "0"

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to a Bool.") (asBool s)
    fromSValue x = throwError ("Expected a string but sent " ++ show x)

instance SAMPType Int where
    toSValue = fromString . show

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to an Int.") (asIntegral s)
    fromSValue x = throwError ("Expected a string but sent " ++ show x)

instance SAMPType Integer where
    toSValue = fromString . show

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to an Integer.") (asIntegral s)
    fromSValue x = throwError ("Expected a string but sent " ++ show x)

instance SAMPType SAMPMapValue where
    toSValue = SAMPMap
    fromSValue (SAMPMap xs) = return xs
    fromSValue x = throwError ("Expected a SAMP map but sent " ++ show x)

instance {-# OVERLAPPABLE #-} (SAMPType a) => SAMPType [a] where
    toSValue = SAMPList . map toSValue
    fromSValue (SAMPList xs) = mapM fromSValue xs
    fromSValue x = throwError ("Expected a SAMP list but sent " ++ show x)

{-

<SAMP float> ::= [ <sign> ] <float-digits>
                 [ "e" | "E" [ <sign> ] <digits> ]

and there is no min/max value specified, so only support
Double values.

-}
instance SAMPType Double where
    toSValue = fromString . show

    fromSValue (SAMPString s) = maybeToM ("Unable to convert " ++ show s ++ " to an Integer.") (asFloating s)
    fromSValue x = throwError ("Expected a string but sent " ++ show x)


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
     scSecret :: HubSecret, -- ^ the SAMP secret, from the hub file
     scHubURL :: String, -- ^ the URL of the server
     scPrivateKey :: ClientSecret,
     -- ^ the private key assigned to the client by the hub
     scHubId :: RString, -- ^ the name of the hub
     scId :: ClientName -- ^ the name of the client (assigned by the hub)
     } deriving (Eq, Show)

{-|
The response from a recipient to a sender.

Use the 'isSAMPSuccess', 'isSAMPError' and 'isSAMPWarning' routines to
query the status of the response. The 'toSAMPResponse', 'toSAMPResponseError'
and 'toSAMPResponseWarning' routines are used to create @SAMPResponse@
values.

The response includes the @samp.status@, @samp.result@, and @samp.error@
fields, as well as any other values (with a key) to be passed around.
-}
data SAMPResponse =
    SROkay { _srokResult :: SAMPMapValue
           , _srokOther :: SAMPMapValue }
        |
    SRWarning { _srwarnResult :: SAMPMapValue
              , _srwarnError :: (RString, SAMPMapValue)
              , _srwarnOther :: SAMPMapValue }
        |
    SRError { _srerrError :: (RString, SAMPMapValue)
            , _srerrOther :: SAMPMapValue }
    deriving (Eq, Show)


toSM :: [SAMPMapElement] -> SAMPMapValue
toSM = M.fromList

instance SAMPType SAMPResponse where

    toSValue (SROkay vals other) =
      SAMPMap (toSM ([ (sStatus, sOkVal)
                     , (sResult, SAMPMap vals)])
               `M.union` other)
      
    toSValue (SRWarning vals (emsg, evals) other) =
      SAMPMap (toSM ([ (sStatus, sWarnVal)
                     , (sResult, SAMPMap vals)
                     , (sError, SAMPMap err)])
               `M.union` other)
      where
        err = M.insert sErrorTxt (SAMPString emsg) evals
              
    toSValue (SRError (emsg, evals) other) =
      SAMPMap (toSM ([ (sStatus, sErrVal)
                     , (sError, SAMPMap err)])
               `M.union` other)
      where
        err = M.insert sErrorTxt (SAMPString emsg) evals

    -- how much should this be rewritten now using an actual map?
    fromSValue (SAMPMap ms) = do
        (status, ms2) <- grabKeyE sStatus ms
        let leftovers = cleanKeys [sResult, sError] ms2

        let getError = do
              (evals, _) <- grabKeyE sError ms2
              grabKeyE sErrorTxt evals

        case status :: RString of
          "samp.ok" -> do
              vals <- getKey sResult ms2
              return (SROkay vals leftovers)
                      
          "samp.warning" -> do
              vals <- getKey sResult ms2
              err <- getError
              return (SRWarning vals err leftovers)

          "samp.error" -> do
              err <- getError
              return (SRError err leftovers)
                     
          _ -> throwError ("Unexpected samp.status of " ++ show status)
                    
    fromSValue x = throwError ("Expected a SAMP map but sent " ++ show x)
                 
-- | Create a SAMP response that indicates success
toSAMPResponse ::
    SAMPMapValue -- ^ key,value pairs to reply to the caller
    -> SAMPMapValue -- ^ extra key/value pairs to send
    -> SAMPResponse
toSAMPResponse = SROkay

-- | Create a SAMP response that indicates an error
toSAMPResponseError ::
    RString -- ^ the error test (@samp.errortxt@)
    -> SAMPMapValue -- ^ other elements of the error
    -> SAMPMapValue -- ^ extra key/value pairs to send
    -> SAMPResponse
toSAMPResponseError emsg evals = SRError (emsg, evals)

-- | Create a SAMP response that indicates a warning.
toSAMPResponseWarning ::
    SAMPMapValue -- ^ successful key,value pairs
    -> RString -- ^ error message (@samp.errortxt@)
    -> SAMPMapValue -- ^ other elements of the error
    -> SAMPMapValue -- ^ extra key/value pairs to send
    -> SAMPResponse
toSAMPResponseWarning svals emsg evals =
    SRWarning svals (emsg, evals) 

-- TODO: perhaps samp.ok/warning/error should be a separate
--       type (e.g. enumerated) so that it's easier to pattern match
--
sStatus , sResult , sOkay, sWarning, sError , sErrorTxt :: RString
sStatus = "samp.status"
sResult = "samp.result"
sOkay = "samp.ok"
sWarning = "samp.warning"
sError  = "samp.error"
sErrorTxt = "samp.errortxt"

sOkVal , sErrVal , sWarnVal :: SAMPValue
sOkVal   = toSValue sOkay
sErrVal  = toSValue sError
sWarnVal = toSValue sWarning

-- | Convert via @SAMPValue@
instance XmlRpcType SAMPResponse where
    toValue = toValue . toSValue
    fromValue xs = fromValue xs >>= fromSValue
    getType _ = TStruct

{-|
Does the response indicate success? Note that a warning will 
return 'True' here; use 'isSAMPWarning' for an explicit check
of this case.
-}
isSAMPSuccess :: SAMPResponse -> Bool
isSAMPSuccess SROkay {} = True
isSAMPSuccess SRWarning {} = True
isSAMPSuccess SRError {} = False
                              
{-|
Does the response indicate an error? Note that a warning will 
return 'True' here; use 'isSAMPWarning' for an explicit check
of this case.
-}
isSAMPError :: SAMPResponse -> Bool
isSAMPError SROkay {} = False
isSAMPError SRWarning {} = True
isSAMPError SRError {} = True

{-|
Does the response indicate an error? Unlike 'isSAMPError' this 
returns 'False' if the response was a warning.
-}
isSAMPErrorOnly :: SAMPResponse -> Bool
isSAMPErrorOnly SROkay {} = False
isSAMPErrorOnly SRWarning {} = False
isSAMPErrorOnly SRError {} = True

-- | Does the response indicate a warning?
isSAMPWarning :: SAMPResponse -> Bool
isSAMPWarning SROkay {} = False
isSAMPWarning SRWarning {} = True
isSAMPWarning SRError {} = False

-- | Return the result stored in a SAMP response.
--
--   This does not return the extra values.
getSAMPResponseResult :: SAMPResponse -> Maybe SAMPMapValue
getSAMPResponseResult (SROkay r _) = Just r
getSAMPResponseResult (SRWarning r _ _) = Just r
getSAMPResponseResult SRError {} = Nothing
                                     
{-|
Return the error information stored in a SAMP response.
The first element of the tuple is the @samp.errortxt@
value, the second element is the other values of the error map.
-}
getSAMPResponseError :: SAMPResponse -> Maybe (RString, SAMPMapValue)
getSAMPResponseError SROkay {} = Nothing
getSAMPResponseError (SRWarning _ e _) = Just e
getSAMPResponseError (SRError e _) = Just e
                        
{-|
Return the contents of the @samp.errortxt@ return value,
if provided.
-}
getSAMPResponseErrorTxt :: SAMPResponse -> Maybe RString
getSAMPResponseErrorTxt = fmap fst . getSAMPResponseError

{-|
Return any "extra" values that were included in the response.
-}
getSAMPResponseExtra :: SAMPResponse -> SAMPMapValue
getSAMPResponseExtra (SROkay _ o) = o
getSAMPResponseExtra (SRWarning _ _ o) = o
getSAMPResponseExtra (SRError _ o) = o
                          
{-|
A SAMP message, which contains the message type ('MType'),
the message parameters as a list of key,value pairs,
and any extra parameters
-}
data SAMPMessage = SM {
      _smType :: MType
    , _smParams :: SAMPMapValue
    , _smExtra ::  SAMPMapValue
    } deriving (Eq, Show)

{-|
Constructor for a 'SAMPMessage'.
-}
toSAMPMessage ::
    (Monad m)
    => MType
    -- ^ The 'MType' of the message (this is the @samp.mtype@ key). It
    --   can not contain a wild card.
    -> SAMPMapValue
    -- ^ The parameters for the message (this is the @samp.params@ key).
    -> SAMPMapValue
    -- ^ Any extra key and value pairs to include in the message.
    -> Err m SAMPMessage
toSAMPMessage mtype params extra
    | isMTWildCard mtype = throwError "MType can not contain a wild card when creating a SAMP message."
    | otherwise          = return (SM mtype params extra)

toSAMPMessageMT ::
    MType
    -- ^ The 'MType' of the message (this is the @samp.mtype@ key). It
    --   MUST NOT contain a wild card, but this is not enforced
    -> SAMPMapValue
    -- ^ The parameters for the message (this is the @samp.params@ key).
    -> SAMPMapValue
    -- ^ Any extra key and value pairs to include in the message.
    -> SAMPMessage
toSAMPMessageMT = SM

-- | Return the 'MType' of the message (the @samp.mtype@ key).
getSAMPMessageType :: SAMPMessage -> MType
getSAMPMessageType = _smType

-- | Return the parameters of the message (the @samp.params@ key).
getSAMPMessageParams :: SAMPMessage -> SAMPMapValue
getSAMPMessageParams = _smParams

-- | Return the extra key,value pairs in this message (i.e. those
--   not part of the @samp.params@ key).
getSAMPMessageExtra :: SAMPMessage -> SAMPMapValue
getSAMPMessageExtra = _smExtra

smtype , sparams :: RString
smtype = "samp.mtype"
sparams = "samp.params"

instance SAMPType SAMPMessage where
    toSValue sm =
      SAMPMap (toSM ([ (smtype, toSValue (_smType sm))
                     , (sparams, SAMPMap (_smParams sm))
                     ]) `M.union` _smExtra sm)
                        
    fromSValue (SAMPMap xs) = do
      (mt, xs2) <- grabKeyE smtype xs
      (ps, xs3) <- grabKeyE sparams xs2
      toSAMPMessage mt ps xs3
      
    fromSValue x = throwError ("Expected a SAMP map but sent " ++ show x)

-- | Convert via @SAMPValue@                   
instance XmlRpcType SAMPMessage where
    toValue = toValue . toSValue
    fromValue xs = fromValue xs >>= fromSValue
                   
    getType _ = TStruct


{-|
A SAMP method call. Consists of a method name and a list of  
parameters.
-}
data SAMPMethodCall = SAMPMethodCall MType [SAMPValue]
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
  -- TODO: maybe just use toMTypeE here, so that the invalid characters
  --       are displayed?
  ns <- maybeToM ("Unable to convert SAMP method name: " ++ n) (toMType n)
  SAMPMethodCall ns `liftM` mapM fromValue vs

fromSMC :: SAMPMethodCall -> MethodCall
fromSMC (SAMPMethodCall n vs) = MethodCall (fromMType n) (map toValue vs)

toSMR :: (Monad m) => MethodResponse -> Err m SAMPMethodResponse
toSMR (Return vs) = SAMPReturn `liftM` fromValue vs
toSMR (Fault _ msg) = return (SAMPFault msg)

-- | The integer fault value is not specified in the SAMP document, so use a
--   value of 1.
fromSMR :: SAMPMethodResponse -> MethodResponse
fromSMR (SAMPReturn vs) = Return (toValue vs)
fromSMR (SAMPFault msg) = Fault 1 msg

-- | Parses a SAMP method call from the XmlRpc input.
parseSAMPCall ::
    (Show e, MonadError e m)
    => String -- ^ XmlRpc input
    -> Err m SAMPMethodCall
parseSAMPCall c = parseCall c >>= toSMC

-- | Parses a SAMP method response.
parseSAMPResponse ::
    (Show e, MonadError e m)
    => String -- ^ XmlRpc input
    -> Err m SAMPMethodResponse
parseSAMPResponse c = parseResponse c >>= toSMR

renderSAMPCall :: SAMPMethodCall -> L.ByteString
renderSAMPCall = renderCall . fromSMC

renderSAMPResponse :: SAMPMethodResponse -> L.ByteString
renderSAMPResponse = renderResponse . fromSMR 

-- | A message tag, which is used to identify messages between a
--   client and the hub. These are semantically distinct from
--   message ids.
--
--   Note that an `IsString` instance is not provided for this type,
--   to make sure that potential conversion errors are handled.
--
newtype MessageTag = MTag { _unMTag :: RString }
    deriving (Eq, Ord)

toMessageTag :: RString -> MessageTag
toMessageTag = MTag

fromMessageTag :: MessageTag -> RString
fromMessageTag = _unMTag
               
instance Show MessageTag where
    show = show . _unMTag

instance SAMPType MessageTag where
    toSValue = SAMPString . fromMessageTag
    fromSValue x = toMessageTag <$> fromSValue x

-- | A message id, which is used to identify messages between
--   the hub and a recipient (on behalf of a client). These
--   are semantically distinct from message tags.
--
--   Note that an `IsString` instance is not provided for this type,
--   to make sure that potential conversion errors are handled.
--
newtype MessageId = MId { _unMId :: RString }
    deriving (Eq, Ord)

toMessageId :: RString -> MessageId
toMessageId = MId

fromMessageId :: MessageId -> RString
fromMessageId = _unMId

instance Show MessageId where
    show = show . _unMId

instance SAMPType MessageId where
    toSValue = SAMPString . fromMessageId
    fromSValue x = toMessageId <$> fromSValue x
           
                                 
-- | The public identifier for a client, which is created by the hub.
--   This is used to refer to other clients. It is *not* the value
--   of the @samp.name@ field of the metadata for the client.
--
newtype ClientName = CN { _unCN :: RString }
    deriving (Eq, Ord)

{-|
The conversion provided by this instance is unsafe:
'error' is called for those strings that contain invalid
characters.
-}
instance IsString ClientName where
  fromString = CN . toRS

fromClientName :: ClientName -> RString
fromClientName = _unCN

toClientName :: RString -> ClientName
toClientName = CN

instance Show ClientName where
    show = show . _unCN
               
instance SAMPType ClientName where
    toSValue = toSValue . fromClientName
    fromSValue s = toClientName <$> fromSValue s

                   
-- | The secret value for a client, which is created by the hub.
--   It is used when communicating with the hub from the client,
--   and should not be known by other agents.
--
--   Note that an `IsString` instance is not provided for this type,
--   to make sure that potential conversion errors are handled.
--
newtype ClientSecret = CS { _unCS :: RString }
    deriving (Eq, Ord)

fromClientSecret :: ClientSecret -> RString
fromClientSecret = _unCS

toClientSecret :: RString -> ClientSecret
toClientSecret = CS 

instance Show ClientSecret where
    show = show . _unCS

instance SAMPType ClientSecret where
    toSValue = toSValue . fromClientSecret
    fromSValue s = toClientSecret <$> fromSValue s

-- | The secret value for a hub.
--   It is used by a client when registering with a hub.
--
--   Note that an `IsString` instance is not provided for this type,
--   to make sure that potential conversion errors are handled.
--
newtype HubSecret = HS { _unHS :: RString }
    deriving Eq

fromHubSecret :: HubSecret -> RString
fromHubSecret = _unHS

toHubSecret :: RString -> HubSecret
toHubSecret = HS 

instance Show HubSecret where
    show = show . _unHS

instance SAMPType HubSecret where
    toSValue = toSValue . fromHubSecret
    fromSValue s = toHubSecret <$> fromSValue s


-- | Create a "nice" output; this is intended for quick debugging
--   and is very limited. I am sure there's a library out there
--   that could do this easier.
dumpSAMPValue :: SAMPValue -> String
dumpSAMPValue = run 0
  where
    run l v = intercalate "\n" (reverse (go l [] v))
    
    indent l s = replicate (l*4) ' ' ++ s

    -- maybe convert a SAMPValue to a list and can then
    -- just fold over it.
    
    go l old (SAMPString s) = indent l (show s) : old
    go l old (SAMPList ls) =
      let l2 = l + 1
          new = map (run l2) ls
          start = indent l "["
          end = indent l "]"
      in end : reverse new ++ (start : old)
    go l old (SAMPMap ms) =
      let l2 = l + 1
          new = map (goKV l2 []) (M.toList ms)
          start = indent l "{"
          end = indent l "}"
      in end : reverse new ++ (start : old)

    goKV l old (k, v) =
      let l2 = l + 1
          out = run l2 v  -- nopity nope nope
          new = indent l (show k ++ " : " ++ out) : old
      in intercalate "\n" (reverse new)
