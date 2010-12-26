{-
Test out the SAMP client code.

  ghc -Wall --make -o testclient -threaded -hide-package monads-fd TestClient.hs
  ./testclient +RTS -N2 -RTS

At the moment it doesn't work with monads-fd:

  ghc -Wall --make -o testclient -threaded -hide-package mtl TestClient.hs


TODO:

 a) work with monads-fd
 b) try out error conditions (like the hub disappearing)
 c) does it make sense to use something like a monad
    transformer for combining IO and Either
 d) register for hub shutting-down message

-}

module Main where

import qualified Control.Exception as CE

import System.Exit (exitFailure, exitSuccess)
import System.IO.Error
import Control.Monad (forM_)
import Control.Monad.Error (catchError)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.ParallelIO.Global

import Network.XmlRpc.Internals (Err, handleError, ioErrorToErr)

import SAMP.Client

main :: IO ()
main = doIt2

doIt :: IO ()
doIt = do
     hubInfo <- getHubInfo
     case hubInfo of
          Nothing -> putStrLn "No SAMP hub found." >> exitFailure
          Just hi -> putStrLn "Found hub." >> processHub hi >> exitSuccess

doIt2 :: IO ()
doIt2 = let act = getHubInfo2 >>= \hi -> liftIO (putStrLn "Found hub.") >> processHub2 hi
            hdlr :: CE.SomeException -> IO ()
            hdlr e = do
                   let ioe :: Maybe CE.IOException
                       ioe = CE.fromException e
                       emsg = case ioe of
                            Just et -> if isUserError et then ioeGetErrorString et else show et
                            _ -> show e
                   putStrLn $ "ERROR: " ++ emsg
                   exitFailure
        in handleError fail act
           `CE.catch` hdlr 
          

processHub :: SampInfo -> IO ()
processHub (ss, surl) = do
           putStrLn $ "Samp secret: " ++ ss
           putStrLn $ "Samp hub:    " ++ surl
           cl <- makeClient (ss, surl)
           case cl of 
                Nothing -> exitFailure
                Just sc -> CE.catch (doClient sc) $ \e -> do
                                    putStrLn $ "Caught error - cleaning up from " ++ show (e :: CE.AsyncException)
                                    unregisterClient sc

           exitSuccess

{-
I was hoping to keep everything within the Err IO monad
but I want to catch asynchronous errors here (in particular
UserInterrupt) so that I can ensure I unregister the client.

This looks like it needs to be done in the IO monad, which
messes some things up. 

Perhaps I want a finally/try rather than catch here?
-}

processHub2 :: SampInfo -> Err IO ()
processHub2 (ss, surl) = do
           liftIO $ putStrLn $ "Samp secret: " ++ ss
           liftIO $ putStrLn $ "Samp hub:    " ++ surl
           cl <- makeClient2 (ss, surl)

           let hdlr :: CE.AsyncException -> IO ()
               hdlr CE.UserInterrupt = handleError fail (unregisterClient2 cl) >> CE.throwIO CE.UserInterrupt
               hdlr e = CE.throwIO e
                   
           -- could use CE.catchJust here
           liftIO $ handleError (\m -> handleError fail (unregisterClient2 cl) >> fail m) (doClient2 cl) `CE.catch` hdlr

           unregisterClient2 cl
           liftIO $ putStrLn "Unregistered client"

{-
Wait until the hub tells us it's about to shutdown.
-}

waitForShutdown2 :: SampClient -> Err IO ()
waitForShutdown2 cl = do
  setXmlrpcCallback2 cl "http://127.0.0.1:8080/" -- need to set up a URL
  liftIO $ putStrLn "About to register subcription to samp.hub.event.unregister"
  declareSubscriptionsSimple2 cl ["samp.hub.event.unregister"]
  liftIO $ putStrLn "Done"
  return ()

{-
XXX TODO shouldn't we check that declareMetadata has returned a SAMPSuccess?
-}

makeClient :: SampInfo -> IO (Maybe SampClient)
makeClient ss = do
           cl <- registerClient ss
           case cl of 
                Left te -> putStrLn ("ERROR: unable to register with Hub\n" ++ show te) >> return Nothing
                Right sc -> do
                           _ <- declareMetadata sc "hsamp-test-client" "Test SAMP client using hSAMP." "0.0.1"
                           return $ Just sc

makeClient2 :: SampInfo -> Err IO SampClient
makeClient2 ss = do
           sc <- registerClient2 ss
           _ <- declareMetadata2 sc "hsamp-test-client" "Test SAMP client using hSAMP." "0.0.1"
           return sc

wait :: Int -> IO ()
wait = threadDelay . (1000000 *)

reportIt :: String -> Either TransportError [(String,SAMPValue)] -> IO ()
reportIt lbl (Left te) = putStrLn $ concat ["    ERROR accessing ", lbl, " : ", show te]
reportIt lbl (Right msgs) = do
         putStrLn $ "    " ++ lbl
         forM_ msgs $ \(n,v) -> putStrLn $ concat ["        ", n, " : ", showSAMPValue v]

reportIt2 :: String -> [(String,SAMPValue)] -> Err IO ()
reportIt2 lbl msgs = do
         liftIO $ putStrLn $ "    " ++ lbl
         forM_ msgs $ \(n,v) -> liftIO $ putStrLn $ concat ["        ", n, " : ", showSAMPValue v]

reportClients :: SampClient -> IO ()
reportClients cl = do
              resp <- getRegisteredClients cl
              case resp of
                   Left te -> putStrLn $ "Unable to query hub: " ++ show te
                   Right ns -> do
                         putStrLn $ concat ["*** Found ", show (length ns), " clients"]
                         forM_ (zip ([1..]::[Int]) ns) $ \(n,name) -> do
                               putStrLn $ concat ["   ", show n, " : ", name]
                               subs <- getSubscriptions cl name
                               reportIt "Subscriptions" subs
                               mds <- getMetadata cl name
                               reportIt "Metadata" mds
                               return ()

reportClients2 :: SampClient -> Err IO ()
reportClients2 cl = do
              ns <- getRegisteredClients2 cl
              liftIO $ putStrLn $ concat ["*** Found ", show (length ns), " clients"]
              forM_ (zip ([1..]::[Int]) ns) $ \(n,name) -> do
                    liftIO $ putStrLn $ concat ["   ", show n, " : ", name]
                    subs <- getSubscriptions2 cl name
                    reportIt2 "Subscriptions" subs
                    mds <- getMetadata2 cl name
                    reportIt2 "Metadata" mds
                    return ()

reportSubscriptions :: SampClient -> String -> IO ()
reportSubscriptions cl msg = do
                    msgs <- getSubscribedClients cl msg
                    reportIt ("Subscriptions to " ++ msg) msgs

reportSubscriptions2 :: SampClient -> String -> Err IO ()
reportSubscriptions2 cl msg = do
                    msgs <- getSubscribedClients2 cl msg
                    reportIt2 ("Subscriptions to " ++ msg) msgs

pingItems :: SampClient -> IO ()
pingItems cl = do
     putStrLn "Calling clients that respond to samp.app.ping"
     msgs <- getSubscribedClients cl "samp.app.ping"
     case msgs of
         Left te -> putStrLn $ concat ["Error qyerying hub for subscriptions to samp.app.ping\n", show te]
         Right rsp -> do
              parallel_ (map (callPing . fst) rsp) >> stopGlobalPool
              putStrLn "Finished calling samp.app.ping"

         where
           callPing p = do
             ret <- callAndWait cl p "samp.app.ping" [] 10
             case ret of
               Left re -> putStrLn $ "ERROR calling " ++ p ++ "\n" ++ show re
               Right (SAMPSuccess _) -> putStrLn $ "Successfuly called " ++ p
               Right (SAMPError emsg _) -> putStrLn $ "ERROR calling " ++ p ++ "\n" ++ emsg
               Right (SAMPWarning wmsg _ _) -> putStrLn $ "WARNING calling " ++ p ++ "\n" ++ wmsg

pingItems2 :: SampClient -> Err IO ()
pingItems2 cl = do
     liftIO $ putStrLn "Calling clients that respond to samp.app.ping"
     rsp <- getSubscribedClients2 cl "samp.app.ping"
     liftIO $ parallel_ (map cp rsp)
     liftIO stopGlobalPool
     liftIO $ putStrLn "Finished calling samp.app.ping"

         where
           cp r = handleError return (callPing (fst r)) >>= putStrLn
           callPing p = do
             ret <- callAndWait2 cl p "samp.app.ping" [] 10
             case ret of
               (SAMPSuccess _) -> return $ "Successfuly called " ++ p
               (SAMPError emsg _) -> return $ "ERROR calling " ++ p ++ "\n" ++ emsg
               (SAMPWarning wmsg _ _) -> return $ "WARNING calling " ++ p ++ "\n" ++ wmsg

doClient :: SampClient -> IO ()
doClient cl = do
         putStrLn $ "Registered client: public name = " ++ sampId cl
         reportClients cl
         putStrLn ""
         reportSubscriptions cl "samp.app.ping"
         reportSubscriptions cl "foo.bar"
         pingItems cl
         wait 10
         unregisterClient cl
         putStrLn "Unregistered client"

doClient2 :: SampClient -> Err IO ()
doClient2 cl = do
         liftIO $ putStrLn $ "Registered client: public name = " ++ sampId cl
         reportClients2 cl
         liftIO $ putStrLn ""
         reportSubscriptions2 cl "samp.app.ping"
         reportSubscriptions2 cl "foo.bar"
         pingItems2 cl
         waitForShutdown2 cl
         liftIO $ wait 10

