{-
Test out the SAMP client code.

TODO:

 a) work with monads-fd
 b) try out error conditions (like the hub disappearing)

A simple TCP server at 
  http://sequence.complete.org/node/258
may be useful reading.

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

import SAMP.Standard.Types
import SAMP.Standard.Client
import SAMP.Standard.Server

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

eConf :: Conf
eConf = nullConf { port = 0 }

processHub :: SAMPInfo -> Err IO ()
processHub si@(ss, surl) = do
           putLn $ "Samp secret: " ++ show ss
           putLn $ "Samp hub:    " ++ show surl

           tid <- liftIO myThreadId

           -- create a socket for the server
           sock <- liftIO $ bindPort eConf
           pNum <- liftIO $ socketPort sock           

           cl <- makeClient si pNum

           -- _ <- liftIO $ forkIO $ runServer (sock,pNum) tid cl

           let hdlr :: CE.AsyncException -> IO ()
               hdlr CE.UserInterrupt = handleError fail (unregisterE cl) >> CE.throwIO CE.UserInterrupt
               hdlr e = CE.throwIO e

               act = liftIO (forkIO (runServer (sock,pNum) tid cl)) >> doClient cl pNum

           -- could use CE.catchJust here
           liftIO $ handleError (\m -> handleError fail (unregisterE cl) >> fail m) act `CE.catch` hdlr

           unregisterE cl
           putLn "Unregistered client"

hostName :: PortNumber -> String
hostName portNum = "http://127.0.0.1:" ++ show portNum ++ "/"

-- an unsafe routine
tRS :: String -> RString
tRS = fromJust . toRString

makeClient :: SAMPInfo -> PortNumber -> Err IO SAMPConnection
makeClient si pNum = do
    cl <- registerClientE si
    let md = toMetadata (tRS "hsamp-test-client")
                        (Just (tRS "Test SAMP client using hSAMP."))
                        Nothing
                        (Just (tRS (hostName pNum ++ "icon.png")))
                        Nothing
    declareMetadataE cl $ (fromJust (toRString "internal.version"), toSValue (fromJust (toRString "0.0.1"))) : md
    return cl

{-
Should really make sure that the server has started up before registering
the connection with the hub.
-}

pingMT , shutdownMT :: MType
pingMT = fromJust $ toMType "samp.app.ping"
shutdownMT = fromJust $ toMType "samp.hub.event.shutdown"

setSubscriptions :: SAMPConnection -> PortNumber -> Err IO ()
setSubscriptions cl pNum = do
  setXmlrpcCallbackE cl $ tRS $ hostName pNum ++ "xmlrpc"
  putLn "About to register subcriptions"
  declareSubscriptionsSimpleE cl [pingMT, shutdownMT]
  putLn "Done"

wait :: Int -> IO ()
wait = threadDelay . (1000000 *)

reportIt :: String -> [SAMPKeyValue] -> Err IO ()
reportIt lbl msgs = do
         putLn $ "    " ++ lbl
         forM_ msgs $ \(n,v) -> putLn $ concat ["        ", show n, " : ", showSAMPValue v]

reportClients :: SAMPConnection -> Err IO ()
reportClients cl = do
              ns <- getRegisteredClientsE cl
              putLn $ concat ["*** Found ", show (length ns), " clients"]
              forM_ (zip ([1..]::[Int]) ns) $ \(n,name) -> do
                    putLn $ concat ["   ", show n, " : ", show name]
                    subs <- getSubscriptionsE cl name
                    reportIt "Subscriptions" subs
                    mds <- getMetadataE cl name
                    reportIt "Metadata" mds

reportSubscriptions :: SAMPConnection -> MType -> Err IO ()
reportSubscriptions cl msg = do
                    msgs <- getSubscribedClientsE cl msg
                    reportIt ("Subscriptions to " ++ show msg) msgs

pingMsg :: (Monad m) => Err m SAMPMessage
pingMsg = toSAMPMessage pingMT [] []

{-
TODO: try asynchronous ping
-}

pingItems :: SAMPConnection -> Err IO ()
pingItems cl = do
     putLn "Calling clients that respond to samp.app.ping (synchronous)"
     rsp <- getSubscribedClientsE cl pingMT
     liftIO $ parallel_ (map cp rsp)
     liftIO stopGlobalPool
     putLn "Finished calling samp.app.ping (synchronous)"

         where
           cp r = handleError return (callPing (fst r)) >>= putStrLn
           callPing p = do
             pmsg <- pingMsg
             rsp <- callAndWaitE cl p pmsg (Just 10)
             let pn = show p
                 etxt = show (fromJust (getSAMPResponseErrorTxt rsp))
             if isSAMPWarning rsp
               then return $ "WARNING pinging " ++ pn ++ "\n" ++ etxt
               else if isSAMPSuccess rsp
                 then return $ "Successfuly pinged " ++ pn
                 else return $ "ERROR pinging " ++ pn ++ "\n" ++ etxt

doClient :: SAMPConnection -> PortNumber -> Err IO ()
doClient cl pNum = do
         putLn $ "Registered client: public name = " ++ show (sampId cl)
         pingE cl
         putLn "Was able to ping the hub using the Standard profile's ping method"
         reportClients cl
         putLn ""
         reportSubscriptions cl pingMT
         reportSubscriptions cl $ fromJust $ toMType "foo.bar"
         pingItems cl
         setSubscriptions cl pNum
         liftIO $ wait 10

runServer :: (Socket,PortNumber) -> ThreadId -> SAMPConnection -> IO ()
runServer (sock,portNum) tid si = do
          putStrLn $ "Starting server at port " ++ show portNum
          simpleHTTPWithSocket sock (nullConf { port = fromEnum portNum }) (handlers tid si)

handlers :: ThreadId -> SAMPConnection -> ServerPart Response
handlers tid si = msum [handleXmlRpc tid si, handleIcon, anyRequest (notFound (toResponse "unknown request"))]

methodList :: ThreadId -> SAMPConnection -> SAMPMethodMap
methodList tid si =
           [("samp.client.receiveNotification", fun (receiveNotification tid si)),
           ("samp.client.receiveCall", fun (receiveCall si)),
           ("samp.client.receiveResponse", fun (receiveResponse si))
           ]

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

notifications :: [(MType, ThreadId -> SAMPConnection -> RString -> RString -> [SAMPKeyValue] -> IO ())]
notifications = [(shutdownMT, handleShutdown)]

calls :: [(MType, SAMPConnection -> RString -> RString -> RString -> [SAMPKeyValue] -> IO ())]
calls = [(pingMT, handlePing)]

responses :: [(MType, SAMPConnection -> RString -> RString -> RString -> [SAMPKeyValue] -> IO ())]
responses = []

handleShutdown :: ThreadId -> SAMPConnection -> RString -> RString -> [SAMPKeyValue] -> IO ()
handleShutdown tid _ _ name _ = do
               putStrLn $ "Received a shutdown message from " ++ show name
               killThread tid

handlePing :: SAMPConnection -> RString -> RString -> RString -> [SAMPKeyValue] -> IO ()
handlePing si _ senderid msgid _ = do
               putStrLn $ "Received a ping request from " ++ show senderid
               _ <- handleError (const (return ())) $ replyE si msgid emptyResponse
               return ()

mt , sp :: RString
mt = fromJust $ toRString "samp.mtype"
sp = fromJust $ toRString "samp.params"

{-
QUS: should receiveNotification/Call be returning IO SAMPServerResult?
-}

sStringToMT :: SAMPValue -> Maybe MType
sStringToMT (SAMPString s) = toMType (fromRString s)
sStringToMT _ = Nothing

emptyResponse :: SAMPResponse
emptyResponse = toSAMPResponse []

{-
TODO:
  better error handling
-}
receiveNotification :: ThreadId -> SAMPConnection -> RString -> RString -> [SAMPKeyValue] -> IO Int
receiveNotification tid si secret senderid struct = do
    sm <- handleError fail (fromSValue (SAMPMap struct) :: Err IO SAMPMessage)
    let mtype = getSAMPMessageType sm
        mparams = getSAMPMessageParams sm
    putStrLn $ "Notification: sent mtype " ++ show mtype ++ " by " ++ show senderid
    case lookup mtype notifications of
      Just func -> func tid si secret senderid mparams >> return 0
      _ -> do
           liftIO $ debugM "SAMP" $ "Unrecognized mtype for notification: " ++ show mtype
           fail $ "Unrecognized mtype for notification: " ++ show mtype

{-
TODO: clean up receiveCall and receiveResponse
-}
-- this one needs to send a response to the hub
--
receiveCall :: SAMPConnection -> RString -> RString -> RString -> [SAMPKeyValue] -> IO Int
receiveCall si secret senderid msgid struct = do
    sm <- handleError fail (fromSValue (SAMPMap struct) :: Err IO SAMPMessage)
    let mtype = getSAMPMessageType sm
        mparams = getSAMPMessageParams sm
    putStrLn $ "Call: sent mtype " ++ show mtype ++ " by " ++ show senderid
    case lookup mtype calls of
      Just func -> func si secret senderid msgid mparams >> return 0
      _ -> do
           liftIO $ debugM "SAMP" $ "Unrecognized mtype for call: " ++ show mtype
           fail $ "Unrecognized mtype for call: " ++ show mtype

receiveResponse :: SAMPConnection -> RString -> RString -> RString -> [SAMPKeyValue] -> IO Int
receiveResponse si secret receiverid msgid struct = do
    sm <- handleError fail (fromSValue (SAMPMap struct) :: Err IO SAMPMessage)
    let mtype = getSAMPMessageType sm
        mparams = getSAMPMessageParams sm
    putStrLn $ "Call: sent mtype " ++ show mtype ++ " by " ++ show receiverid
    case lookup mtype calls of
      Just func -> func si secret receiverid msgid mparams >> return 0
      _ -> do
           liftIO $ debugM "SAMP" $ "Unrecognized mtype for response: " ++ show mtype
           fail $ "Unrecognized mtype for response: " ++ show mtype

-- this could be done by server (to some degree anyway?)

{-
TODO:
improve the error handling
-}

getResponse :: ThreadId -> SAMPConnection -> RqBody -> ServerPart Response
getResponse tid si (Body bdy) = do
    liftIO $ debugM "SAMP" $ "SAMP body of call from Hub is:\n" ++ (L.unpack bdy)

    -- _ <- liftIO $ server (methodList tid si) (L.unpack bdy)
    -- return is a bytestring which encodes any error information
    -- since server catches this and does the transformation
    -- do we need to do anything other than just repsond as a 200?
    -- ok ""

    let act = parseSAMPCall (L.unpack bdy) >>= methods (methodList tid si)
    _ <- liftIO (handleError fail act)
    ok (toResponse "")


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

