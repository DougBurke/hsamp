> {-# LANGUAGE OverloadedStrings #-}

Set up a simple SAMP access point

> module Server 
>        (
>         getSocket, getAddress, hostName, hostNameBS,
>         runServer
>        ) where
>
> import Control.Applicative ((<|>))
> import Control.Monad.Trans (liftIO)
>
> import qualified Data.ByteString.Char8 as B
> import qualified Data.ByteString.Lazy.Char8 as L
>
> import qualified Network as N
> import Network.Socket (Socket, PortNumber, socketPort)
> import Snap.Types
> import Snap.Http.Server

Return a socket with a randomly-chosen port for use by the server.

> getSocket :: IO Socket
> getSocket = N.listenOn (N.PortNumber (fromIntegral (0::Int)))

> getAddress :: Socket -> IO String
> getAddress = fmap hostName . socketPort

The location of the server we create to handle incoming messages from
the SAMP hub. Note that this includes the XML-RPC route since in this
case we do not support any other access.

> hostName :: PortNumber -> String
> hostName portNum = "http://127.0.0.1:" ++ show portNum ++ "/xmlrpc"

> hostNameBS :: PortNumber -> B.ByteString
> hostNameBS = B.pack . hostName

Creates the server, listening to the assigned socket/port.

> runServer :: Socket -> (String -> IO ()) -> IO ()
> runServer socket processCall = do
>      portNum <- socketPort socket
>      httpServe
>        "*" -- is this correct?
>        (fromEnum portNum)
>        (hostNameBS portNum)
>        Nothing -- access log
>        Nothing -- error log
>        $ handleXmlRpc processCall <|> noRequest

> noRequest :: Snap ()
> noRequest = do
>     modifyResponse $ setResponseCode 404
>     writeBS "404 Not Found"
>     r <- getResponse
>     finishWith r

XML-RPC is done via a HTML POST of an XML document.
At present I enforce the content-type restriction but
this may be a bit OTT.

> xmlct :: B.ByteString
> xmlct = "text/xml"

TODO: report on the invalid content-type?

> invalidCT :: Snap ()
> invalidCT = do
>     modifyResponse $ setResponseCode 400
>     writeBS "400 Bad Request"
>     r <- getResponse
>     finishWith r

Should we allow processCall to return some form of error?

> handleXmlRpc :: (String -> IO ()) -> Snap ()
> handleXmlRpc processCall = method POST $ dir "xmlrpc" $ do
>     r <- getRequest
>     let cType = getHeader "content-type" r
>     if cType == Just xmlct
>       then getRequestBody >>= liftIO . processCall . L.unpack >> modifyResponse (setResponseCode 200)
>       else invalidCT

