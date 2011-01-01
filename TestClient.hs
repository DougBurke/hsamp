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
import Control.Monad (msum, forM_, unless)
import Control.Monad.Error (catchError)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (ThreadId, killThread, threadDelay, forkIO, myThreadId)
import Control.Concurrent.ParallelIO.Global

import Network.XmlRpc.Internals (Value(..), Err, handleError, ioErrorToErr, parseResponse, parseCall)
import Network.XmlRpc.Server (XmlRpcMethod, fun, methods)

import SAMP.Client

import Happstack.Server.SimpleHTTP
import Happstack.Server.HTTP.FileServe
import Happstack.Server.MessageWrap

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Maybe (fromJust)

{-
Todo: need to send hub info to the web server, which means we need
to start that after finding the hub.
-}

main :: IO ()
main = do
     tid <- myThreadId
     _ <- forkIO $ runServer pNum tid
     doIt
     exitSuccess

doIt :: IO ()
doIt = let act = getHubInfoE >>= \hi -> liftIO (putStrLn "Found hub.") >> processHub hi

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

       in handleError fail act
           `CE.catches` [CE.Handler ioHdlr, CE.Handler asyncHdlr, CE.Handler otherHdlr]
          

{-
I was hoping to keep everything within the Err IO monad
but I want to catch asynchronous errors here (in particular
UserInterrupt) so that I can ensure I unregister the client.

This looks like it needs to be done in the IO monad, which
messes some things up. 

Perhaps I want a finally/try rather than catch here?
-}

processHub :: SampInfo -> Err IO ()
processHub (ss, surl) = do
           liftIO $ putStrLn $ "Samp secret: " ++ ss
           liftIO $ putStrLn $ "Samp hub:    " ++ surl
           cl <- makeClient (ss, surl)

           let hdlr :: CE.AsyncException -> IO ()
               hdlr CE.UserInterrupt = handleError fail (unregisterClientE cl) >> CE.throwIO CE.UserInterrupt
               hdlr e = CE.throwIO e
                   
           -- could use CE.catchJust here
           liftIO $ handleError (\m -> handleError fail (unregisterClientE cl) >> fail m) (doClient cl) `CE.catch` hdlr

           unregisterClientE cl
           liftIO $ putStrLn "Unregistered client"

{-
Wait until the hub tells us it's about to shutdown.
-}

pNum :: Int
pNum = 12345

hostName :: Int -> String
hostName portNum = "http://127.0.0.1:" ++ show portNum ++ "/"

{-
Should really make sure that the server has started up before registering
the connection with the hub.
-}
waitForShutdown :: SampClient -> Err IO ()
waitForShutdown cl = do
  setXmlrpcCallbackE cl $ hostName pNum ++ "xmlrpc"
  liftIO $ putStrLn "About to register subcription to samp.hub.event.shutdown"
  declareSubscriptionsSimpleE cl ["samp.hub.event.shutdown"]
  liftIO $ putStrLn "Done"
  return ()

{-
XXX TODO shouldn't we check that declareMetadata has returned a SAMPSuccess?
-}

makeClient :: SampInfo -> Err IO SampClient
makeClient ss = do
           sc <- registerClientE ss
           _ <- declareMetadataE sc "hsamp-test-client" "Test SAMP client using hSAMP." "0.0.1" [("samp.icon.url", ValueString (hostName pNum ++ "icon.png"))]
           return sc

wait :: Int -> IO ()
wait = threadDelay . (1000000 *)

reportIt :: String -> [(String,SAMPValue)] -> Err IO ()
reportIt lbl msgs = do
         liftIO $ putStrLn $ "    " ++ lbl
         forM_ msgs $ \(n,v) -> liftIO $ putStrLn $ concat ["        ", n, " : ", showSAMPValue v]

reportClients :: SampClient -> Err IO ()
reportClients cl = do
              ns <- getRegisteredClientsE cl
              liftIO $ putStrLn $ concat ["*** Found ", show (length ns), " clients"]
              forM_ (zip ([1..]::[Int]) ns) $ \(n,name) -> do
                    liftIO $ putStrLn $ concat ["   ", show n, " : ", name]
                    subs <- getSubscriptionsE cl name
                    reportIt "Subscriptions" subs
                    mds <- getMetadataE cl name
                    reportIt "Metadata" mds
                    return ()

reportSubscriptions :: SampClient -> String -> Err IO ()
reportSubscriptions cl msg = do
                    msgs <- getSubscribedClientsE cl msg
                    reportIt ("Subscriptions to " ++ msg) msgs

pingItems :: SampClient -> Err IO ()
pingItems cl = do
     liftIO $ putStrLn "Calling clients that respond to samp.app.ping"
     rsp <- getSubscribedClientsE cl "samp.app.ping"
     liftIO $ parallel_ (map cp rsp)
     liftIO stopGlobalPool
     liftIO $ putStrLn "Finished calling samp.app.ping"

         where
           cp r = handleError return (callPing (fst r)) >>= putStrLn
           callPing p = do
             ret <- callAndWaitE cl p "samp.app.ping" [] 10
             case ret of
               (SAMPSuccess _) -> return $ "Successfuly called " ++ p
               (SAMPError emsg _) -> return $ "ERROR calling " ++ p ++ "\n" ++ emsg
               (SAMPWarning wmsg _ _) -> return $ "WARNING calling " ++ p ++ "\n" ++ wmsg

doClient :: SampClient -> Err IO ()
doClient cl = do
         liftIO $ putStrLn $ "Registered client: public name = " ++ sampId cl
         reportClients cl
         liftIO $ putStrLn ""
         reportSubscriptions cl "samp.app.ping"
         reportSubscriptions cl "foo.bar"
         pingItems cl
         waitForShutdown cl
         liftIO $ wait 10

runServer :: Int -> ThreadId -> IO ()
runServer portNum tid = do
          putStrLn $ "Starting server at port " ++ show portNum
          simpleHTTP (nullConf { port =  portNum }) (handlers tid)

handlers :: ThreadId -> ServerPart Response
handlers tid = msum [handleXmlRpc tid, handleIcon, anyRequest (notFound (toResponse "unknown request"))]

methodList :: ThreadId -> [(String, XmlRpcMethod)]
methodList tid = [("samp.client.receiveNotification", fun (receiveNotification tid))]

{-
From SAMP 1.2 document, callable clients must support

nb a hidden first argument of string private-key

receiveNotification(string sender-id, map message)

Method called by the hub when dispatching a notification to its recip-
ient. The form of the message map is given in Section 3.8.

receiveCall(string sender-id, string msg-id, map message)

Method called by the hub when dispatching a call to its recipient. The
client MUST at some later time make a matching call to reply() on the
hub. The form of the message map is given in Section 3.8.

receiveResponse(string responder-id, string msg-tag, map response)

Method used by the hub to dispatch to the sender the response of an
earlier asynchronous call. The form of the response map is given in
Section 3.9.

-}

{-
TODO:

could throw an error in 'Err m String ' monad
so that there is some info about failure, or
we catch such errors and return the necessary
info for an error return.
-}
lookupJ :: String -> [(String, Value)] -> Value
lookupJ s = fromJust . lookup s

{-
We fake a return value for the moment since there is no
XmlRpcType encoding of (). I think, for notifications, no
response is expected, but the SAMP 1.2 document is somewhat
unclear from a quick read.

TODO: dispatch on mtype
check that secret is correct; should we also worry about the name
field?

-}
receiveNotification :: ThreadId -> String -> String -> [(String, Value)] -> IO Int
receiveNotification tid secret name struct = do
                    let mtype = lookupJ "samp.mtype" struct
                        mparams = lookupJ "samp.params" struct
                    liftIO $ putStrLn "In receive Notification"
                    liftIO $ putStrLn $ "  secret = " ++ secret
                    liftIO $ putStrLn $ "  name   = " ++ name
                    liftIO $ putStrLn $ "  mtype  = " ++ show mtype
                    liftIO $ putStrLn $ "  mparams = " ++ show mparams
                    killThread tid
                    return 1

getResponse :: ThreadId -> RqBody -> ServerPart ()
getResponse tid (Body bdy) = do
     mCall <- handleError fail $ parseCall (L.unpack bdy) -- better error handling needed
     liftIO $ putStrLn "*** START CALL"
     liftIO $ print mCall
     liftIO $ putStrLn "*** END CALL"
     -- ans <- handleError fail $ methods (methodList tid) mCall -- better error handling needed
     ans <- liftIO $ handleError fail $ methods (methodList tid) mCall -- better error handling needed
     liftIO $ putStrLn "*** START ANSWER"
     liftIO $ print ans
     liftIO $ putStrLn "*** END ANSWER"
     return ()

xmlct :: B.ByteString
xmlct = B.pack "text/xml"

-- XmlRpc is done via a HTML POST of an XML document
--
handleXmlRpc :: ThreadId -> ServerPart Response
handleXmlRpc tid = dir "xmlrpc" $ do
    methodM POST
    cType <- getHeaderM "content-type"
    unless (cType == Just xmlct) $ escape (badRequest (toResponse "Invalid content type")) -- not sure what to do here
    -- decodeBody myPolicy
    r <- askRq
    rsp <- getResponse tid (rqBody r)
    return (toResponse "Foo") -- what to return here?

{-
This is for the un-released Happstack
myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 4096 1024
-}

handleIcon :: ServerPart Response
handleIcon = dir "icon.png" $ liftIO (putStrLn "Served icon") >> serveFile (asContentType "image/png") "public/icon.png"

