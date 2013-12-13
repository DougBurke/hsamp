{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2011, 2013
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------
-}

module Main where

import Control.Monad.Trans (liftIO)

import Network.SAMP.Standard

-- We rely here on the OverloadedStrings extension to
-- convert from Strings to the required types, but explicit
-- conversions are also provided by routines like
-- @toMTypeE@ and @stringToKeyValE@.
--

main :: IO ()
main = withSAMP $ runE $ do

    -- Read information from lockfile to locate and register with hub.
    conn <- getHubInfoE >>= registerClientE 
    liftIO $ putStrLn $ "Hub id:    " ++ fromRString (scHubId conn)
    liftIO $ putStrLn $ "Client id: " ++ fromRString (scId conn)
    
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
    msg <- toSAMPMessage "file.load" [("filename", "/tmp/foo.bar")]
    notifyAllE_ conn msg

    -- Unregister
    liftIO $ putStrLn "Unregistering from the hub"
    unregisterE conn
