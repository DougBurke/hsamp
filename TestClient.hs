{-
Test out the SAMP client code.

  ghc -Wall --make -o testclient -threaded -hide-package monads-fd TestClient.hs
  ./testclient +RTS -N2 -RTS

At the moment it doesn't work with monads-fd:

  ghc -Wall --make -o testclient -hide-package mtl TestClient.hs

-}

module Main where

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import Control.Concurrent.ParallelIO.Global

import SAMP.Client

main :: IO ()
main = do
     hubInfo <- getHubInfo
     case hubInfo of
          Nothing -> putStrLn "No SAMP hub found." >> exitFailure
          Just hi -> putStrLn "Found hub." >> processHub hi

processHub :: SampInfo -> IO ()
processHub (ss, surl) = do
           putStrLn $ "Samp secret: " ++ ss
           putStrLn $ "Samp hub:    " ++ surl
           cl <- makeClient (ss, surl)
           case cl of 
                Nothing -> exitFailure
                Just sc -> doClient sc
           exitSuccess

makeClient :: SampInfo -> IO (Maybe SampClient)
makeClient ss = do
           cl <- registerClient ss
           case cl of 
                Left te -> putStrLn ("ERROR: unable to register with Hub\n" ++ show te) >> return Nothing
                Right sc -> do
                           _ <- declareMetadata sc "hsamp-test-client" "Test SAMP client using hSAMP." "0.0.1"
                           return $ Just sc

wait :: Int -> IO ()
wait = threadDelay . (1000000 *)

reportIt :: String -> Either TransportError [(String,SAMPValue)] -> IO ()
reportIt lbl (Left te) = putStrLn $ concat ["    ERROR accessing ", lbl, " : ", show te]
reportIt lbl (Right msgs) = do
         putStrLn $ concat ["    ", lbl]
         forM_ msgs $ \(n,v) -> putStrLn $ concat ["        ", n, " : ", showSAMPValue v]

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

reportSubscriptions :: SampClient -> String -> IO ()
reportSubscriptions cl msg = do
                    msgs <- getSubscribedClients cl msg
                    reportIt ("Subscriptions to " ++ msg) msgs

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

doClient :: SampClient -> IO ()
doClient cl = do
         putStrLn $ concat ["Registered client: public name = ", sampId cl]
         reportClients cl
         putStrLn ""
         reportSubscriptions cl "samp.app.ping"
         reportSubscriptions cl "foo.bar"
         pingItems cl
         wait 10
         unregisterClient cl
         putStrLn "Unregistered client"
