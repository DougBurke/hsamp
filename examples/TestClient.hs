{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2011, 2013, 2015, 2016
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------
-}

{-
Test out the SAMP client code.

TODO:

 - combine code with example code (e.g. Snooper.hs)
 - possibly remove, as maybe better off using the other example
   codes
-}

module Main (main) where

import qualified Control.Exception as CE
import qualified Data.Map as M

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, isUserError)

import Control.Arrow (first)
import Control.Concurrent (ThreadId, killThread, threadDelay, forkIO, myThreadId)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, unless, void)
import Control.Monad.Trans (liftIO)

import Data.Maybe (fromJust, fromMaybe)
import Data.Version (showVersion)

import Network.SAMP.Standard.Client (callAndWaitE
                                    , declareMetadataE
                                    , declareSubscriptionsSimpleE
                                    , getMetadataE
                                    , getRegisteredClientsE
                                    , getSubscribedClientsE
                                    , getSubscriptionsE
                                    , pingE
                                    , registerClientE
                                    , toMetadataE
                                    , unregisterE)
import Network.SAMP.Standard.Server (SAMPCallFunc
                                    , SAMPCallMap
                                    , SAMPNotificationFunc
                                    , SAMPNotificationMap
                                    , SAMPResponseFunc
                                    , callAllE
                                    , setXmlrpcCallbackE
                                    , simpleClientServer)
import Network.SAMP.Standard.Setup (getHubInfoE
                                   , sampLockFileE)
import Network.SAMP.Standard.Server.Scotty (runServer)
import Network.SAMP.Standard.Types (Err
                                   , MType
                                   , RString
                                   , SAMPConnection(..)
                                   , SAMPInfo
                                   , SAMPMessage
                                   , SAMPValue
                                   , fromClientName
                                   , getSAMPInfoHubMetadata
                                   , getSAMPInfoHubSecret
                                   , getSAMPInfoHubURL
                                   , getSAMPResponseErrorTxt
                                   , handleError
                                   , isSAMPSuccess
                                   , isSAMPWarning
                                   , runE
                                   , showSAMPValue
                                   , toMessageTag
                                   , toRString
                                   , toRStringE
                                   , toSAMPMessage
                                   , toSAMPResponse
                                   , toSValue
                                   , withSAMP)

import Network.Socket (PortNumber, socketPort)

import System.Log.Logger (Priority(DEBUG), setLevel, updateGlobalLogger)

import Utils (getSocket)

import Paths_hsamp (version)

main :: IO ()
main = do
     args <- getArgs
     _ <- case args of
            ["1"] -> updateGlobalLogger "SAMP" (setLevel DEBUG)
            ["0"] -> return ()
            [] -> return ()
            _ -> do
              pName <- getProgName
              hPutStrLn stderr ("Usage: " ++ pName ++ " [0|1]")
              exitFailure

     doIt
     exitSuccess

putLn :: String -> Err IO ()
putLn = liftIO . putStrLn

{-
ThreadKilled is thrown in processHub, so it should be caught there
rather than the whole routine.
-}
doIt :: IO ()
doIt =
  let act = sampLockFileE >>=
            getHubInfoE >>=
            \hi -> putLn "Found hub." >>
            processHub hi

      hdlr :: String -> IO ()
      hdlr msg  = hPutStrLn stderr ("ERROR: " ++ msg) >> exitFailure

      -- could probably do this with a single hander
      ioHdlr :: CE.IOException -> IO ()
      ioHdlr e = let emsg = if isUserError e then ioeGetErrorString e else show e
                 in hdlr emsg

      -- this assumes the only way to get a ThreadKilled is via a shutdown message
      asyncHdlr :: CE.AsyncException -> IO ()
      asyncHdlr e = let emsg = if e == CE.ThreadKilled then "SAMP Hub has shut down." else show e
                    in hdlr emsg

      otherHdlr :: CE.SomeException -> IO ()
      otherHdlr e = hdlr (show e)

      handlers = [CE.Handler ioHdlr, CE.Handler asyncHdlr, CE.Handler otherHdlr]

  in withSAMP (runE act `CE.catches` handlers)

{-
I was hoping to keep everything within the Err IO monad
but I want to catch asynchronous errors here (in particular
UserInterrupt) so that I can ensure I unregister the client.

This looks like it needs to be done in the IO monad, which
messes some things up. 

Perhaps I want a finally/try rather than catch here?
-}

processHub :: SAMPInfo -> Err IO ()
processHub si = do
    let ss = getSAMPInfoHubSecret si
        surl = getSAMPInfoHubURL si
        ekeys = M.toList (getSAMPInfoHubMetadata si)
    putLn ("Samp secret: " ++ show ss)
    putLn ("Samp hub:    " ++ surl)
    unless (null ekeys) $ do
         putLn "  extra info:"
         forM_ ekeys (\(k,v) -> putLn ("   " ++ k ++ " -> " ++ v))

    tid <- liftIO myThreadId

    -- create a socket for the server
    sock <- liftIO getSocket
    pNum <- liftIO (socketPort sock)

    cl <- makeClient si pNum

    let hdlr :: CE.AsyncException -> IO ()
        hdlr CE.UserInterrupt = runE (unregisterE cl)
                                >> CE.throwIO CE.UserInterrupt
        hdlr e = CE.throwIO e

        doServer = runServer sock undefined (processCall tid cl)
        act = liftIO (forkIO doServer) >> doClient cl pNum

    -- could use CE.catchJust here
    liftIO $ handleError (\m -> runE (unregisterE cl) >> fail m) act `CE.catch` hdlr

    unregisterE cl
    putLn "Unregistered client"

hostName :: PortNumber -> String
hostName portNum = "http://127.0.0.1:" ++ show portNum ++ "/"

-- an unsafe routine
tRS :: String -> RString
tRS = fromJust . toRString

makeClient :: SAMPInfo -> PortNumber -> Err IO SAMPConnection
makeClient si _ = do
    conn <- registerClientE si
    md <- toMetadataE "hsamp-test-client"
          (Just "Test SAMP client using hSAMP.")
          Nothing
          Nothing  -- (Just (hostName pNum ++ "icon.png"))
          Nothing
    let v = fromMaybe "unknown" (toRString (showVersion version))
        mds = M.insert "testclient.version" (toSValue v) md
    declareMetadataE conn mds
    return conn

{-
Should really make sure that the server has started up before registering
the connection with the hub.
-}

setSubscriptions :: SAMPConnection -> PortNumber -> Err IO ()
setSubscriptions cl pNum = do
  setXmlrpcCallbackE cl $ tRS (hostName pNum ++ "xmlrpc")
  putLn "About to register subcriptions"
  declareSubscriptionsSimpleE cl ["samp.app.ping", "samp.hub.event.shutdown"]
  putLn "Finished registering subscriptions"

wait :: Int -> IO ()
wait = threadDelay . (1000000 *)

reportIt :: Show k => String -> [(k, SAMPValue)] -> Err IO ()
reportIt lbl msgs = do
  putLn ("    " ++ lbl)
  let showME (n, v) =
        putLn (concat ["        ", show n, " : ", showSAMPValue v])
  forM_ msgs showME

reportClients :: SAMPConnection -> Err IO ()
reportClients cl = do
    ns <- getRegisteredClientsE cl
    putLn (concat ["*** Found ", show (length ns), " clients"])
    forM_ (zip ([1..]::[Int]) ns) $ \(n,name) -> do
          putLn (concat ["   ", show n, " : ", show name])
          subs <- getSubscriptionsE cl name
          reportIt "Subscriptions" subs
          mds <- getMetadataE cl name
          reportIt "Metadata" (M.toList mds)

reportSubscriptions :: SAMPConnection -> MType -> Err IO ()
reportSubscriptions cl msg = do
    msgs <- getSubscribedClientsE cl msg
    let msgs' = map (first fromClientName) msgs
    reportIt ("Subscriptions to " ++ show msg) msgs'

pingMsg :: (Monad m) => Err m SAMPMessage
pingMsg = toSAMPMessage "samp.app.ping" M.empty M.empty

{-
TODO: try asynchronous ping
-}

pingItems :: SAMPConnection -> Err IO ()
pingItems cl = do
    putLn "Calling clients that respond to samp.app.ping (synchronous)"
    rsp <- getSubscribedClientsE cl "samp.app.ping"
    liftIO (void (mapConcurrently cp rsp))
    putLn "Finished calling samp.app.ping (synchronous)"

        where
          cp r = handleError return (callPing (fst r)) >>= putStrLn
          callPing p = do
            pmsg <- pingMsg
            rsp <- callAndWaitE cl p pmsg (Just 10)
            let pn = show p
                etxt = show (fromJust (getSAMPResponseErrorTxt rsp))
            if isSAMPWarning rsp
              then return ("WARNING pinging " ++ pn ++ "\n" ++ etxt)
              else if isSAMPSuccess rsp
                then return ("Successfuly pinged " ++ pn)
                else return ("ERROR pinging " ++ pn ++ "\n" ++ etxt)

pingItemsAsync :: SAMPConnection -> Err IO ()
pingItemsAsync cl = do
    putLn "Calling clients that respond to samp.app.ping (asynchronous)"
    msg <- pingMsg
    -- TODO: could generate a randomized tag
    msgTag <- toMessageTag <$> toRStringE "need-a-better-system"
    rsp <- callAllE cl msgTag msg
    let showRsp (n, mid) = putStrLn ("  contacted " ++ show n ++
                                     " with id " ++ show mid)
    liftIO (forM_ rsp showRsp)
    
doClient :: SAMPConnection -> PortNumber -> Err IO ()
doClient conn pNum = do
    putLn ("Registered client: public name = " ++ show (scId conn))
    setSubscriptions conn pNum
    pingE conn
    putLn "Was able to ping the hub using the Standard profile's ping method"
    reportClients conn
    putLn ""
    reportSubscriptions conn "samp.app.ping"
    reportSubscriptions conn "foo.bar"
    pingItems conn
    pingItemsAsync conn
    liftIO (putStrLn "Sleeping for 10 seconds." >> wait 10)

-- handle a request from the SAMP Hub
processCall ::
    ThreadId
    -> SAMPConnection
    -> String
    -> IO ()
processCall tid conn = 
    simpleClientServer conn (notifications tid) calls rfunc

notifications :: ThreadId -> SAMPNotificationMap
notifications tid = [("samp.hub.event.shutdown", handleShutdown tid)]

calls :: SAMPCallMap
calls = [("samp.app.ping", handlePing)]

handleShutdown :: ThreadId -> SAMPNotificationFunc
handleShutdown tid _ name _ = do
    putStrLn ("Received a shutdown message from " ++ show name)
    killThread tid

handlePing :: SAMPCallFunc
handlePing _ senderid msgid _ = do
    putStrLn ("Received a ping request from " ++ show senderid ++
              " msgid=" ++ show msgid)
    return (toSAMPResponse M.empty M.empty)

rfunc :: SAMPResponseFunc
rfunc _ receiverid msgid rsp =
    if isSAMPSuccess rsp
      then putStrLn ("Got a response to msg-id=" ++ show msgid ++
                     " from=" ++ show receiverid)
      else putStrLn ("ERROR: " ++
                     show (fromJust (getSAMPResponseErrorTxt rsp)))

