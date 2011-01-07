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
           putLn $ "Samp hub:    " ++ surl

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
  putLn "Finished registering subscriptions"

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
pingMsg = toSAMPMessage pingMT []

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

pingItemsAsync :: SAMPConnection -> Err IO ()
pingItemsAsync cl = do
     putLn "Calling clients that respond to samp.app.ping (asynchronous)"
     msg <- pingMsg
     let msgid = fromJust (toRString "need-a-better-system")
     rsp <- callAllE cl msgid msg
     liftIO $ forM_ rsp $ \(n,mid) -> putStrLn ("  contacted " ++ show n ++ " with id " ++ show mid)

doClient :: SAMPConnection -> PortNumber -> Err IO ()
doClient cl pNum = do
         putLn $ "Registered client: public name = " ++ show (sampId cl)
         setSubscriptions cl pNum
         pingE cl
         putLn "Was able to ping the hub using the Standard profile's ping method"
         reportClients cl
         putLn ""
         reportSubscriptions cl pingMT
         reportSubscriptions cl $ fromJust $ toMType "foo.bar"
         pingItems cl
         pingItemsAsync cl
         liftIO $ putStrLn "Sleeping for 10 seconds." >> wait 10

runServer :: (Socket,PortNumber) -> ThreadId -> SAMPConnection -> IO ()
runServer (sock,portNum) tid si = do
          putStrLn $ "Starting server at port " ++ show portNum
          simpleHTTPWithSocket sock (nullConf { port = fromEnum portNum }) (handlers tid si)

handlers :: ThreadId -> SAMPConnection -> ServerPart Response
handlers tid si = msum [handleXmlRpc tid si, handleIcon, anyRequest (notFound (toResponse "unknown request"))]

notifications :: ThreadId -> [SAMPNotificationFunc]
notifications tid = [(shutdownMT, handleShutdown tid)]

calls :: [SAMPCallFunc]
calls = [(pingMT, handlePing)]

handleShutdown :: ThreadId  -> RString -> RString -> [SAMPKeyValue] -> IO ()
handleShutdown tid _ name _ = do
               putStrLn $ "Received a shutdown message from " ++ show name
               killThread tid

handlePing :: RString -> RString -> RString -> [SAMPKeyValue] -> IO SAMPResponse
handlePing _ senderid msgid _ = do
    putStrLn $ "Received a ping request from " ++ show senderid ++ " msgid=" ++ show msgid
    return $ toSAMPResponse []

rfunc :: SAMPResponseFunc
-- rfunc secret receiverid msgid rsp =
rfunc _ receiverid msgid rsp =
    if isSAMPSuccess rsp
      then do
             putStrLn $ "Got a response to msg=" ++ show msgid ++ " from=" ++ show receiverid
             return ()
      else putStrLn $ "ERROR: " ++ show (fromJust (getSAMPResponseErrorTxt rsp))

getResponse :: ThreadId -> SAMPConnection -> RqBody -> ServerPart Response
getResponse tid si (Body bdy) = do
    let call = L.unpack bdy
    liftIO $ simpleServer si (notifications tid) calls rfunc call
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

