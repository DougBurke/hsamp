{-# LANGUAGE OverloadedStrings #-}

{-
Test out the SAMP client code.

TODO:

 a) work with monads-fd
 b) try out error conditions (like the hub disappearing)

-}

module Main where

import qualified Control.Exception as CE

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO.Error
import Control.Monad (msum, forM_, unless)
-- import Control.Monad.Error (catchError)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (ThreadId, killThread, threadDelay, forkIO, myThreadId)
import Control.Concurrent.ParallelIO.Global

import Network.SAMP.Standard

import Network.Socket (Socket, PortNumber, socketPort)
import Happstack.Server.SimpleHTTP
import Happstack.Server.HTTP.FileServe
-- import Happstack.Server.MessageWrap

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import System.Log.Logger

import Data.Maybe (fromJust)

main :: IO ()
main = do
     args <- getArgs
     _ <- case args of
            ["1"] -> updateGlobalLogger "SAMP" (setLevel DEBUG)
            ["0"] -> return ()
            [] -> return ()
            _ -> do
              pName <- getProgName
              putStrLn $ "Usage: " ++ pName ++ " [0|1]"
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
doIt = let act = getHubInfoE >>= \hi -> putLn "Found hub." >> processHub hi

           -- could probably do this with a single hander
           ioHdlr :: CE.IOException -> IO ()
           ioHdlr e = let emsg = if isUserError e then ioeGetErrorString e else show e
                      in putStrLn ("ERROR: " ++ emsg) >> exitFailure

           -- this assumes the only way to get a ThreadKilled is via a shutdown message
           asyncHdlr :: CE.AsyncException -> IO ()
           asyncHdlr e = let emsg = if e == CE.ThreadKilled then "SAMP Hub has shut down." else show e
                         in putStrLn ("ERROR: " ++ emsg) >> exitFailure

           otherHdlr :: CE.SomeException -> IO ()
           otherHdlr e = putStrLn ("ERROR: " ++ show e) >> exitFailure

       in withSAMP (runE act
           `CE.catches` [CE.Handler ioHdlr, CE.Handler asyncHdlr, CE.Handler otherHdlr])

{-
I was hoping to keep everything within the Err IO monad
but I want to catch asynchronous errors here (in particular
UserInterrupt) so that I can ensure I unregister the client.

This looks like it needs to be done in the IO monad, which
messes some things up. 

Perhaps I want a finally/try rather than catch here?
-}

eConf :: Conf
eConf = nullConf { port = 0 }

processHub :: SAMPInfo -> Err IO ()
processHub si@(ss, surl, ekeys) = do
           putLn $ "Samp secret: " ++ fromRString ss
           putLn $ "Samp hub:    " ++ surl
           unless (null ekeys) $ do
                putLn "  extra info:"
                forM_ ekeys $ \(k,v) -> putLn $ "   " ++ k ++ " -> " ++ v

           tid <- liftIO myThreadId

           -- create a socket for the server
           sock <- liftIO $ bindPort eConf
           pNum <- liftIO $ socketPort sock           

           cl <- makeClient si pNum

           -- _ <- liftIO $ forkIO $ runServer (sock,pNum) tid cl

           let hdlr :: CE.AsyncException -> IO ()
               hdlr CE.UserInterrupt = runE (unregisterE cl) >> CE.throwIO CE.UserInterrupt
               hdlr e = CE.throwIO e

               act = liftIO (forkIO (runServer (sock,pNum) tid cl)) >> doClient cl pNum

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
makeClient si pNum = do
    conn <- registerClientE si
    md <- toMetadataE "hsamp-test-client"
          (Just "Test SAMP client using hSAMP.")
          Nothing
          (Just (hostName pNum ++ "icon.png"))
          Nothing
    declareMetadataE conn $ ("testclient.version", "0.0.1") : md
    return conn

{-
Should really make sure that the server has started up before registering
the connection with the hub.
-}

setSubscriptions :: SAMPConnection -> PortNumber -> Err IO ()
setSubscriptions cl pNum = do
  setXmlrpcCallbackE cl $ tRS $ hostName pNum ++ "xmlrpc"
  putLn "About to register subcriptions"
  declareSubscriptionsSimpleE cl ["samp.app.ping", "samp.hub.event.shutdown"]
  putLn "Finished registering subscriptions"

wait :: Int -> IO ()
wait = threadDelay . (1000000 *)

reportIt :: String -> [SAMPKeyValue] -> Err IO ()
reportIt lbl msgs = do
         putLn $ "    " ++ lbl
         forM_ msgs $ \(n,v) -> putLn $ concat ["        ", fromRString n, " : ", showSAMPValue v]

reportClients :: SAMPConnection -> Err IO ()
reportClients cl = do
              ns <- getRegisteredClientsE cl
              putLn $ concat ["*** Found ", show (length ns), " clients"]
              forM_ (zip ([1..]::[Int]) ns) $ \(n,name) -> do
                    putLn $ concat ["   ", show n, " : ", fromRString name]
                    subs <- getSubscriptionsE cl name
                    reportIt "Subscriptions" subs
                    mds <- getMetadataE cl name
                    reportIt "Metadata" mds

reportSubscriptions :: SAMPConnection -> MType -> Err IO ()
reportSubscriptions cl msg = do
                    msgs <- getSubscribedClientsE cl msg
                    reportIt ("Subscriptions to " ++ show msg) msgs

pingMsg :: (Monad m) => Err m SAMPMessage
pingMsg = toSAMPMessage "samp.app.ping" []

{-
TODO: try asynchronous ping
-}

pingItems :: SAMPConnection -> Err IO ()
pingItems cl = do
     putLn "Calling clients that respond to samp.app.ping (synchronous)"
     rsp <- getSubscribedClientsE cl "samp.app.ping"
     liftIO $ parallel_ (map cp rsp)
     liftIO stopGlobalPool
     putLn "Finished calling samp.app.ping (synchronous)"

         where
           cp r = handleError return (callPing (fst r)) >>= putStrLn
           callPing p = do
             pmsg <- pingMsg
             rsp <- callAndWaitE cl p pmsg (Just 10)
             let pn = fromRString p
                 etxt = show (fromJust (getSAMPResponseErrorTxt rsp))
             if isSAMPWarning rsp
               then return $ "WARNING pinging " ++ pn ++ "\n" ++ etxt
               else if isSAMPSuccess rsp
                 then return $ "Successfuly pinged " ++ pn
                 else return $ "ERROR pinging " ++ pn ++ "\n" ++ etxt

pingItemsAsync :: SAMPConnection -> Err IO ()
pingItemsAsync cl = do
     putLn "Calling clients that respond to samp.app.ping (asynchronous)"
     msg <- pingMsg
     msgid <- toRStringE "need-a-better-system"
     rsp <- callAllE cl msgid msg
     liftIO $ forM_ rsp $ \(n,mid) -> putStrLn ("  contacted " ++ fromRString n ++ " with id " ++ showSAMPValue mid)

doClient :: SAMPConnection -> PortNumber -> Err IO ()
doClient conn pNum = do
         putLn $ "Registered client: public name = " ++ fromRString (scId conn)
         setSubscriptions conn pNum
         pingE conn
         putLn "Was able to ping the hub using the Standard profile's ping method"
         reportClients conn
         putLn ""
         reportSubscriptions conn "samp.app.ping"
         reportSubscriptions conn "foo.bar"
         pingItems conn
         pingItemsAsync conn
         liftIO $ putStrLn "Sleeping for 10 seconds." >> wait 10

runServer :: (Socket,PortNumber) -> ThreadId -> SAMPConnection -> IO ()
runServer (sock,portNum) tid si = do
          putStrLn $ "Starting server at port " ++ show portNum
          simpleHTTPWithSocket sock (nullConf { port = fromEnum portNum }) (handlers tid si)

handlers :: ThreadId -> SAMPConnection -> ServerPart Response
handlers tid si = msum
    [handleXmlRpc tid si, handleIcon,
     anyRequest (notFound (toResponse ("unknown request" :: String)))]

notifications :: ThreadId -> [SAMPNotificationFunc]
notifications tid = [("samp.hub.event.shutdown", handleShutdown tid)]

calls :: [SAMPCallFunc]
calls = [("samp.app.ping", handlePing)]

handleShutdown :: ThreadId  -> MType -> RString -> RString -> [SAMPKeyValue] -> IO ()
handleShutdown tid _ _ name _ = do
               putStrLn $ "Received a shutdown message from " ++ fromRString name
               killThread tid

handlePing :: MType -> RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse
handlePing _ _ senderid msgid _ = do
    putStrLn $ "Received a ping request from " ++ fromRString senderid ++ " msgid=" ++ fromRString msgid
    return $ toSAMPResponse []

rfunc :: SAMPResponseFunc
-- rfunc secret receiverid msgid rsp =
rfunc _ receiverid msgid rsp =
    if isSAMPSuccess rsp
      then do
             putStrLn $ "Got a response to msg=" ++ fromRString msgid ++ " from=" ++ fromRString receiverid
             return ()
      else putStrLn $ "ERROR: " ++ show (fromJust (getSAMPResponseErrorTxt rsp))

getResponse :: ThreadId -> SAMPConnection -> RqBody -> ServerPart Response
getResponse tid si (Body bdy) = do
    let call = L.unpack bdy
    liftIO $ simpleClientServer si (notifications tid) calls rfunc call
    ok (toResponse (""::String))

xmlct :: B.ByteString
xmlct = B.pack "text/xml"

-- XmlRpc is done via a HTML POST of an XML document
--
handleXmlRpc :: ThreadId -> SAMPConnection -> ServerPart Response
handleXmlRpc tid si = dir "xmlrpc" $ do
    methodM POST
    cType <- getHeaderM "content-type"
    -- TODO: do we throw a sensible error (if so, how to get it back to the caller)
    -- or just ignore the content type setting?
    unless (cType == Just xmlct) $ escape (badRequest (toResponse ("Invalid content type: " ++ show cType)))
    r <- askRq
    getResponse tid si (rqBody r)

handleIcon :: ServerPart Response
handleIcon = dir "icon.png" $ liftIO (putStrLn "Served icon") >> serveFile (asContentType "image/png") "public/icon.png"

