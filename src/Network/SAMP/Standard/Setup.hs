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
                                   ) where

import qualified Control.Exception as CE

import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)

import Data.List (stripPrefix)

import Network.URI (URI, parseAbsoluteURI)
import Network.XmlRpc.Internals (Err)

import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistErrorType, ioeGetErrorType)

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

    
              
