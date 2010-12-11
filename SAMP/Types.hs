{-
Types for the SAMP code.
-}

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

module SAMP.Types (
       SampClient(..), SampInfo, SampSecret, SampHubURL,
       SampId, SampPrivateKey,
       SAMPValue(..),
       TransportError(..),
       SAMPResponse(..), SAMPReturn,
       toSAMPValue, fromSAMPValue
       ) where

import qualified Control.Arrow as CA
import qualified Control.Monad.Error.Class as ME

import Network.XmlRpc.Internals

import Data.List (intercalate)

type SampSecret = String
type SampHubURL = String
type SampInfo = (SampSecret, SampHubURL)

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
toSAMPValue e = error $ "Unexpected Value: " ++ show e

fromSAMPValue :: SAMPValue -> String
fromSAMPValue (SAMPString s) = s
fromSAMPValue (SAMPList xs)  = "[" ++ intercalate ", " (map fromSAMPValue xs) ++ "]"
fromSAMPValue (SAMPMap m)    = "{" ++ intercalate ", " (map (\(n,v) -> show n ++ " -> " ++ fromSAMPValue v) m) ++ "}"

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

