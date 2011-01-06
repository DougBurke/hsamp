{-
Handle the server side of the SAMP Standard profile.
-}

module SAMP.Standard.Server (

       SAMPMethod, SAMPServerResult,
       SAMPFun, fun,

       SAMPMethodMap,
       server, methods, handleSAMPCall

       ) where

import System.Log.Logger

import Network.XmlRpc.Internals
-- import Network.XmlRpc.Server as S

-- import Control.Monad.Error (throwError)
import Control.Monad.Trans (liftIO)

import qualified Control.Exception as CE

import qualified Data.ByteString.Lazy.Char8 as L

import SAMP.Standard.Types

-- the name of the SAMP client logging instance
sLogger :: String
sLogger = "SAMP.StandardProfile.Server"

-- log the message to the SAMP server logger at the debug level.
dbg :: String -> Err IO ()
dbg = liftIO . debugM sLogger

showException :: CE.SomeException -> String
showException = show

handleIO :: IO a -> Err IO a
handleIO io = liftIO (CE.try io) >>= either (fail . showException) return

-- | The return result
type SAMPServerResult = Err IO SAMPMethodResponse

-- | The type of SAMP methods on the server.
type SAMPMethod = (SAMPMethodCall -> SAMPServerResult)

--
-- Converting Haskell functions to SAMP methods.
-- This is very-heavily inspired by the haxr Server module
--

-- | Turns any function 
--   @(SAMPType t1, ..., SAMPType tn, SAMPType r) => 
--   t1 -> ... -> tn -> IO r@
--   into a 'SAMPMethod'
fun :: SAMPFun a => a -> SAMPMethod
fun = toSAMPFun

class SAMPFun a where
    toSAMPFun :: a -> SAMPMethodCall -> SAMPServerResult

instance SAMPType a => SAMPFun (IO a) where
    toSAMPFun x (SAMPMethodCall _ []) = do
			      v <- handleIO x
			      return (SAMPReturn (toSValue v))
    toSAMPFun _ _ = fail "Too many arguments"

instance (SAMPType a, SAMPFun b) => SAMPFun (a -> b) where
    toSAMPFun f (SAMPMethodCall n (x:xs)) = do
				  v <- fromSValue x
				  toSAMPFun (f v) (SAMPMethodCall n xs)
    toSAMPFun _ _ = fail "Too few arguments"

{-|
Reads a SAMP method call from a string, uses the supplied method
to generate a response and returns that response as a string
-}
handleSAMPCall :: (SAMPMethodCall -> SAMPServerResult) -- ^ method to call
           -> String -- ^ XmlRpc input containing the SAMP details of the call
           -> IO L.ByteString -- ^ response
handleSAMPCall f str = do
    resp <- handleError (return . SAMPFault) (parseSAMPCall str >>= f)
    return $ renderSAMPResponse resp

{-
XXX TODO
Technically we should really have (RString,SAMPMethod) for the
function mappings, but go with String here for now.
-}

{-|
A map from SAMP method name to Haskell routine used to process
the message.
-}
type SAMPMethodMap = [(String, SAMPMethod)]

{-|
Look up the Haskell function to handle the SAMP method call
and execute it.

This routine includes logging to the "SAMP.StandardProfile.Server"
logger (at present only debug-level information).
-}
methods :: SAMPMethodMap -> SAMPMethodCall -> SAMPServerResult
methods xs c@(SAMPMethodCall name _) = do
    let mname = show name
    dbg $ "Executing SAMP method: " ++ mname
    method <- maybeToM ("Unknown SAMP method: " ++ mname) (lookup mname xs)
    rsp <- method c
    dbg $ "Result is:\n" ++ show rsp
    return rsp

-- | A simple SAMP server.
server :: SAMPMethodMap 
       -> String -- ^ the XmlRPC input containing the SAMP details of the call
       -> IO L.ByteString -- ^ response
server t = handleSAMPCall (methods t)

{-
XXX TODO XXX

want to have something to make handling of mtypes easy, a la the methods/server
routine

-}