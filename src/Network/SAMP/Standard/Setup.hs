{-# LANGUAGE CPP #-}

{-|
Module      :  Network.SAMP.Standard.Setup
Copyright   :  (c) Douglas Burke 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr

Utility routines for setting up a client or hub for the
Standard SAMP profile.

-}

module Network.SAMP.Standard.Setup (
                                    sampLockFileE
                                   , getHubInfoE
                                   ) where

import qualified Control.Arrow as CA
import qualified Control.Exception as CE

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import qualified Network.HTTP.Conduit as NC

import qualified System.IO.Strict as S


import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)

import Data.List (stripPrefix)

import Network.SAMP.Standard.Types (SAMPInfo, toSAMPInfo,
                                    removeKeyE, toRStringE,
                                    toHubSecret)
import Network.URI (URI, parseAbsoluteURI, uriPath, uriScheme)
import Network.XmlRpc.Internals (Err)

import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError, isDoesNotExistErrorType,
                        ioeGetErrorType)
import System.Log.Logger (debugM)

    
-- the name of the SAMP client logging instance
cLogger :: String
cLogger = "SAMP.StandardProfile.Setup"

-- log the message to the SAMP client logger at the debug level.
dbg :: String -> Err IO ()
dbg = liftIO . debugM cLogger

homeEnvVar :: String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
homeEnvVar = "USERPROFILE"
#else
homeEnvVar = "HOME"
#endif

getEnvM :: String -> IO (Either () String)
getEnvM key = CE.tryJust
        (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
        (getEnv key)

stdPrefix :: String
stdPrefix = "std-lockurl:"

{-| 
Returns the location of the SAMP lock file, as a URI 
(so @/home/sampuser/.samp@ is returned as 
@file:///home/sampuser/.samp@). This supports the SAMP 1.2 standard,
so look for

  1) The SAMP_HUB environment variable, as long as it starts with the prefix
     @std-lockurl:@.

  2) @$HOME/.samp@ (for UNIX-like systems) and
     @$USERPROFILE/.samp@ for Windows

Note that this routine does /not/ check that the lock file exists.

The behavior of the routine when the SAMP_HUB environment is set but does not
start with @std-lockurl:@ may change.
-}
sampLockFileE :: Err IO URI
sampLockFileE = do
  eHub <- liftIO (getEnvM "SAMP_HUB")
  case eHub of
    Right hub -> findHubIn hub
    Left _ -> findHubInHome


-- | Return the location of the Hub using the "HOME" environment variable
--   (the choice of variable depends on the OS: for UNIX it is @$HOME@
--   and for Windows @$USERPROFILE@.
--
--   This does *NOT* check that the hub file exists.
--
findHubInHome :: Err IO URI
findHubInHome = do
  eHome <- liftIO (getEnvM homeEnvVar)
  case eHome of
    Right home ->
        let fname = home ++ "/.samp"
            emsg = "Unable to convert " ++ fname ++ " to a URL"
            fileURL = parseAbsoluteURI . ("file://" ++)
        in case fileURL fname of
             Just x -> return x
             Nothing -> throwError emsg

    Left _ -> throwError ("Unable to find " ++ homeEnvVar ++
                          " environment variable")

-- | Return the location of the hub given a starting location.
--
--   This does *NOT* check that the hub file exists.
--
findHubIn :: String -> Err IO URI
findHubIn fname =
    let emsg f = "SAMP_HUB does not contain a valid URI: " ++ f
    in  case stripPrefix stdPrefix fname of
          Nothing -> findHubInHome
          Just f -> case parseAbsoluteURI f of
                      Just x -> return x
                      Nothing -> throwError (emsg f)

    
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
isLineTerminator c = c `elem` ("\r\n"::String)

-- Remove un-needed lines from the SAMP lock file
-- and return a list of interesting lines
--
stripLines ::String -> [String]
stripLines = filter (\x ->not (null x || head x == '#')) . splitLines

-- | Given the contents of a SAMP lockfile, return
--   a map containing all the assignments in the file.
--
--   The file is assumed to be correctly formatted, so that any errors
--   in it will result in an exception.
--
processLockFile :: String -> M.Map String String
processLockFile inStr = M.fromList (foldr step [] (stripLines inStr))
    where
        step = (:) . CA.second tail . break (== '=')

-- | Extract the information from the contents of the hub file.
--
extractHubInfoE ::
    Monad m
    => URI                   -- ^ Location of the lock file
    -> M.Map String String   -- ^ key,value pairs from the lock file
    -> Err m SAMPInfo
extractHubInfoE fileLoc ms = do
  (secretStr, xs1) <- removeKeyE "samp.secret" ms
  (url, xs2) <- removeKeyE "samp.hub.xmlrpc.url" xs1
  secret <- toHubSecret <$> toRStringE secretStr
  return (toSAMPInfo fileLoc secret url xs2)

{-|
Return information about the SAMP hub needed by 'registerE' and
'registerClientE'. It also includes any extra key,value pairs
included in the SAMP lock file.

This routine includes logging to the @SAMP.StandardProfile.Client@
logger (at present only debug-level information).

It should only be used after 'Network.SAMP.Standard.Types.withSAMP'
has been called.
-}
getHubInfoE ::
    URI   -- ^ the output of 'sampLockFileE'
    -> Err IO SAMPInfo
getHubInfoE uri = do
    dbg ("Looking for a SAMP lock file using the URI: " ++ show uri)
    cts <- case uriScheme uri of
             "file:" -> readLockFileE uri
             "http:" -> getLockFileE uri
             "https:" -> getLockFileE uri -- untested
             _ -> throwError ("Unsupported URI for the lockfile: " ++
                              show uri)

    dbg "Read contents of lock file."
    let alist = processLockFile cts
    dbg ("Contents: " ++ show alist)
    extractHubInfoE uri alist

-- read in the lock file from disk
readLockFileE :: URI -> Err IO String
readLockFileE uri = do
    let fname = uriPath uri
        errVal e = if isDoesNotExistError e
                   then "Is a SAMP hub running? Unable to read: " ++ fname
                   else show e

    dbg ("About to read in contents of lock file: " ++ fname)
    r <- liftIO (CE.try (S.readFile fname))
    case r of
        Right txt -> return txt
        Left e -> throwError (errVal e)

-- download the lock file
getLockFileE :: URI -> Err IO String
getLockFileE uri = do
    let loc = show uri
    dbg ("About to download contents of lock file via " ++ show loc)
    fmap L.unpack (liftIO (NC.simpleHttp loc))

