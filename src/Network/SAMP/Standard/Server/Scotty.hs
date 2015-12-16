{-# LANGUAGE OverloadedStrings #-}

{-|
Module      :  Network.SAMP.Standard.Server.Scotty
Copyright   :  (c) Douglas Burke 2011, 2013, 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr

Set up a simple SAMP access point using Scotty <https://github.com/scotty-web/scotty>.

Calls to the server that have a missing or invalid @content-type@
setting currently error out. This behaviour /may/ be changed.

-}

module Network.SAMP.Standard.Server.Scotty
       (
         runServer
       , runServerSignal
       ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad.Trans (liftIO)

import Data.Default.Class (def)

import Network.HTTP.Types (status400)
import Network.Socket (Socket)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (Destination(..), destination,
                                             -- logStdoutDev,
                                             mkRequestLogger)

import System.Log.Logger
import System.Log.FastLogger (fromLogStr)

import Web.Scotty

-- the name of the SAMP logging instance
cLogger :: String
cLogger = "SAMP.StandardProfile.Server.Scotty"

-- log the message to the SAMP logger at the debug level.
-- dbg :: (MonadIO m) => String -> m ()
dbg :: String -> ActionM ()
dbg = liftIO . debugM cLogger

{-|
Creates the server, listening to the assigned socket/port.

Should provide more access to the configuration settings.
-}
runServer ::
  Socket -- ^ the socket for the server
  -> String -- ^ the URL of the server (currently unused)
  -> (String -> IO ())
  -- ^ The function is called with the body of the response, which is
  --   expected to be in XML format.
  -> IO ()
runServer socket _ processCall = _server socket processCall Nothing

{-
As 'runServer', but indicates when the server is about to
start (to allow callers that use forkIO to start the server
can be told when the server is ready). This is not ideal
(the MVar is filled before the server itself is actually
started), and is likely to be removed.
-}
runServerSignal ::
  Socket -- ^ the socket for the server
  -> String -- ^ the URL of the server (currently unused)
  -> (String -> IO ())
  -- ^ The function is called with the body of the response, which is
  --   expected to be in XML format.
  -> MVar ()
  -- ^ This is expected to be empty, as it is filled just before
  --   the server is started
  -> IO ()
runServerSignal socket _ processCall mvar =
  _server socket processCall (Just mvar)


_server ::
  Socket -> (String -> IO ()) -> Maybe (MVar ()) -> IO ()
_server socket processCall mmvar = do
  mware <- logRequests

  -- signal caller
  case mmvar of
    Just mvar -> putMVar mvar ()
    _ -> return ()
    
  scottySocket def socket $ do
    -- middleware logStdoutDev
    middleware mware

    post "/xmlrpc/" $ do
      dbg "Processing a post call..."
      cType <- header "content-type"
      dbg ("  Content-Type: " ++ show cType)
      if cType == Just "text/xml"
        then handleXmlRpc processCall
        else invalidCT

    -- assume we get a reasonable 404 page if nothing else matches

{-
The output from Network.Wai.Middleware.RequestLogger.logStdoutDev 
uses the fast-logger package, whereas I have been using hslogger.
The following uses hslogger to handle the logging from
the request logger.
-}

logRequests :: IO Middleware
logRequests =
  let opts = def { destination = Callback logIt }
      logIt = debugM cLogger . B.unpack . fromLogStr
  in mkRequestLogger opts

{-
XML-RPC is done via a HTML POST of an XML document.
At present I enforce the content-type restriction but
this may be a bit OTT.
-}

invalidCT :: ActionM ()
invalidCT = do
  status status400
  text "400 Bad Request (missing or invalid Content-Type header)"

{-
Should we allow processCall to return some form of error?

Also, should it allow invalid (or perhaps just missing)
content-type values?
-}

handleXmlRpc ::
  (String -> IO ()) -- ^ user-defined processing
 -> ActionM ()
handleXmlRpc processCall = do
  b <- body
  liftIO (processCall (L.unpack b))

