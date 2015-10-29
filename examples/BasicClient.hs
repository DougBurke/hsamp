{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2011, 2013, 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------
-}

module Main (main) where

import Control.Monad.Trans (liftIO)

import Network.SAMP.Standard

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import System.Log.Logger (Priority(DEBUG), setLevel, updateGlobalLogger)

-- We rely here on the OverloadedStrings extension to
-- convert from Strings to the required types, but explicit
-- conversions are also provided by routines like
-- @toMTypeE@ and @stringToKeyValE@.
--

usage :: IO ()
usage = do
    n <- getProgName
    hPutStrLn stderr ("Usage: " ++ n ++ " [--debug]")
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["--debug"] -> updateGlobalLogger "SAMP" (setLevel DEBUG)
      [] -> return ()
      _ -> usage
    withSAMP runClient

runClient :: IO ()
runClient = runE $ do

    -- Read information from lockfile to locate and register with hub.
    conn <- sampLockFileE >>= getHubInfoE >>= registerClientE 
    liftIO (putStrLn ("Hub id:    " ++ fromRString (scHubId conn)))
    let clName = fromRString (fromClientName (scId conn))
    liftIO (putStrLn ("Client id: " ++ clName))
    
    -- Store metadata in hub for use by other applications.
    let vkey = ("dummy.version", "0.1-3")
    md <- toMetadataE "dummy" (Just "Test Application")
                      Nothing Nothing Nothing
    declareMetadataE conn (vkey : md)
    liftIO $ putStrLn "Registered with the hub"
    
    -- Send a message requesting file load to all other
    -- registered clients, not wanting any response.
    --
    -- Should really check that /tmp/foo.bar does not exist before
    -- sending this message!
    liftIO $ putStrLn "Sending file.load message for /tmp/foo.bar"
    msg <- toSAMPMessage "file.load" [("filename", "/tmp/foo.bar")] []
    notifyAllE_ conn msg

    -- Unregister
    liftIO $ putStrLn "Unregistering from the hub"
    unregisterE conn
