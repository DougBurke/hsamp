{-# LANGUAGE OverloadedStrings #-}

{-|
Module      :  Network.SAMP.Standard.Server.Snap
Copyright   :  (c) Smithsonian Astrophysical Observatory 2011
License     :  BSD-like

Maintainer  :  dburke@cfa.harvard.edu
Stability   :  unstable
Portability :  requires haxr

Set up a simple SAMP access point using the 
SNAP server <http://snapframework.com/>.

At present it is limited to version 0.2.16.2 of @snap-server@.

Calls to the server that have a missing or invalid @content-type@
setting currently error out. This behaviour /may/ be changed.

-}

module Network.SAMP.Standard.Server.Snap
       (
        runServer
       ) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import Network.Socket (Socket, socketPort)

import Snap.Types
import Snap.Http.Server

{-|
Creates the server, listening to the assigned socket/port.

Should provide more access to the configuration settings. In
version @0.3@ of Snap there's a configuration monad that could
be used.
-}
runServer :: Socket -- ^ the socket for the server (only used to get the port number at present)
             -> String -- ^ the URL of the server
             -> (String -> IO ()) -- ^ this is important, and really needs documentation
             -> IO ()
runServer socket url processCall = do
     portNum <- socketPort socket
     httpServe
       "*" -- is this correct?
       (fromEnum portNum)
       (B.pack url)
       Nothing -- access log
       Nothing -- error log
       $ handleXmlRpc processCall <|> noRequest

noRequest :: Snap ()
noRequest = do
    modifyResponse $ setResponseCode 404
    writeBS "404 Not Found"
    r <- getResponse
    finishWith r

{-
XML-RPC is done via a HTML POST of an XML document.
At present I enforce the content-type restriction but
this may be a bit OTT.
-}

xmlct :: B.ByteString
xmlct = "text/xml"

-- TODO: report on the invalid content-type?

invalidCT :: Snap ()
invalidCT = do
    modifyResponse $ setResponseCode 400
    writeBS "400 Bad Request"
    r <- getResponse
    finishWith r

{-
Should we allow processCall to return some form of error?

Also, should it allow invalid (or perhaps just missing)
content-type values?

-}

handleXmlRpc :: (String -> IO ()) -- ^ user-defined processing
                -> Snap ()
handleXmlRpc processCall = method POST $ dir "xmlrpc" $ do
    r <- getRequest
    let cType = getHeader "content-type" r
    if cType == Just xmlct
        then getRequestBody >>= liftIO . processCall . L.unpack >> modifyResponse (setResponseCode 200)
        else invalidCT

