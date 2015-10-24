{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr, not tested on Windows (will not work due to POSIX file permissions)
------------------------------------------------------------------------

This is only needed whilst the xmlhack flag is being supported. This
module is a replacement for the haxr call routine that ensures that
there are no empty XML elements written using the <name/> form (at
present it only does this for struct elements).

-}

module HubCall (call, emptyResponse, makeResponse) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8  as U

import qualified Network.XmlRpc.Internals as XI

import Network.SAMP.Standard (SAMPValue(SAMPMap)
                             , SAMPMethodResponse(SAMPReturn)
                             , SAMPKeyValue
                             , renderSAMPResponse)
                                       
import qualified System.IO.Streams as Streams
import qualified Network.Http.Client as HC
import qualified Network.URI as NU
import qualified Text.Read.Compat as TRC
import qualified OpenSSL as SSL

import Data.Int (Int64)    
import Data.Maybe (fromMaybe)
    
{-
import qualified Control.Exception as CE

import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT

-- ghc 7.10.2 says that this import is redundant, but if
-- it is commented out then fdWrite and closeFd are not defined
import qualified System.Posix.IO as P
    
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs, getEnv, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (ownerReadMode, ownerWriteMode)
import System.Posix.Types (Fd)
import System.Random (RandomGen, StdGen, getStdGen, setStdGen)
-- import System.Timeout (timeout)

import Control.Arrow (first)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar,
                                putMVar, readMVar, takeMVar)
import Control.Monad (forM, forM_, guard, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)

import Data.Bits ((.|.))
import Data.ByteString.Base64.Lazy (decodeLenient)
import Data.Default.Class (def)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing,
                   listToMaybe, mapMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
    
-- import Data.Version (showVersion)

import Network.SAMP.Standard (MType, RString, SAMPKeyValue
                             , MessageId, toMessageId
                             , MessageTag, fromMessageTag, toMessageTag
                             , SAMPResponse, SAMPMethodCall(..)
                             , SAMPMethodResponse(..)
                             , SAMPValue(..)
                             , ClientName, ClientSecret
                             , toClientName, fromClientName
                             , toClientSecret, fromClientSecret
                             , fromSValue, toSValue
                             , fromRString, toRString
                             , fromMType, toMType, toMTypeE, isMTWildCard
                             , randomAlphaNumRString
                             , renderSAMPResponse, toSAMPResponse
                             , toSAMPResponseError
                             , parseSAMPCall
                             , handleError)
-- import Network.SAMP.Standard.Server.Scotty (runServer)

import qualified Network as N
import Network.Socket (Socket, socketPort)
    
import Network.HTTP.Types (status400)
import Network.URI (URI(..), parseAbsoluteURI, parseURI)

import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (Destination(..), destination,
                                             -- logStdoutDev,
                                             mkRequestLogger)

import System.Log.Logger (Priority(DEBUG, INFO, NOTICE, WARNING),
                          debugM, infoM, noticeM,
                          setLevel, updateGlobalLogger)
import System.Log.FastLogger (fromLogStr)
import System.Posix.IO (createFile)
    
import Web.Scotty (ActionM
                  , body, header, middleware, get, post
                  , json, raw
                  , setHeader, scottySocket, status, text)

import Utils (getSocket)

    -}
     
{-
This is a re-implementation of haxr's XC.call routine, since I have
to edit the XML serialization for ds9.
-}

call :: String
     -> String
     -> [XI.Value]
     -> XI.Err IO XI.Value
call url method args = do
  let req = XI.renderCall (XI.MethodCall method args)
      reqConv = replaceEmptyStruct req 
      -- reqConv = req  -- is ds9 7.4 okay?
  respRaw <- XI.ioErrorToErr (post' url reqConv)
  respParse <- XI.parseResponse (L.unpack respRaw)
  handleResponse respParse

handleResponse :: Monad m => XI.MethodResponse -> m XI.Value
handleResponse (XI.Return v) = return v
handleResponse (XI.Fault code str) = fail ("Error " ++ show code ++ ": " ++ str)

post' :: String -> L.ByteString -> IO U.ByteString
post' url content = do
  uri <- maybeFail ("Bad URI: '" ++ url ++ "'") (NU.parseURI url)
  let a = NU.uriAuthority uri
  auth <- maybeFail ("Bad URI authority: '" ++ show (fmap showAuth a) ++ "'") a
  post_ uri auth content
      where showAuth (NU.URIAuth u r p) = "URIAuth "++u++" "++r++" "++p

post_ :: NU.URI -> NU.URIAuth -> L.ByteString -> IO U.ByteString
post_ uri auth content = SSL.withOpenSSL $ do
    let hostname = B.pack (NU.uriRegName auth)
        port     = fromMaybe 443 (TRC.readMaybe $ drop 1 $ NU.uriPort auth)

    c <- case init $ NU.uriScheme uri of
        "http"  ->
            HC.openConnection hostname port
        "https" -> do
            ctx <- HC.baselineContextSSL
            HC.openConnectionSSL ctx hostname port
        x -> fail ("Unknown scheme: '" ++ x ++ "'!")

    req  <- request uri auth (L.length content)
    body' <- HC.inputStreamBody <$> Streams.fromLazyByteString content

    _ <- HC.sendRequest c req body'

    s <- HC.receiveResponse c $ \resp i -> 
        case HC.getStatusCode resp of
          200 -> readLazyByteString i
          -- 200 -> putStrLn "*** processing response" >> readLazyByteString i
          _   -> fail (show (HC.getStatusCode resp) ++ " " ++ B.unpack (HC.getStatusMessage resp))

    HC.closeConnection c

    return s

readLazyByteString :: Streams.InputStream B.ByteString -> IO U.ByteString
readLazyByteString i = L.fromChunks <$> go
  where
    go :: IO [B.ByteString]
    go = do
      res <- Streams.read i
      case res of
        Nothing -> return []
        Just bs -> (bs:) <$> go

userAgent :: B.ByteString
userAgent = "Haskell XmlRpcClient/0.1 modified for hsamp"
            
-- | Create an XML-RPC compliant HTTP request.
request :: NU.URI -> NU.URIAuth -> Int64 -> IO HC.Request
request uri auth len = HC.buildRequest $ do
    HC.http HC.POST (B.pack $ NU.uriPath uri)
    HC.setContentType "text/xml"
    HC.setContentLength len

    case parseUserInfo auth of
      (Just user, Just pass) -> HC.setAuthorizationBasic (B.pack user) (B.pack pass)
      _                      -> return ()

    -- mapM_ (uncurry setHeader) usrHeaders

    HC.setHeader "User-Agent" userAgent

    where
      parseUserInfo uinfo = let (u,pw) = break (==':') $ NU.uriUserInfo uinfo
                            in ( if null u then Nothing else Just u
                               , if null pw then Nothing else Just (tail pw))

maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return
                
-- end of code from haxr



-- | The \"empty\" return value. Note that this does not return a
--   valid SAMP value (as far as I understand it), since it
--   reurns <param><value/></param>.
--
--   At present this hard codes the response since I am having trouble
--   with ds9 7.3.2, since I guess it doesn't like
--     <param><value><string></string></value></param>
--
--   My reading of http://xmlrpc.scripting.com/spec.html is that
--     <value/> and <value><string/></value>
--   are equivalent, since "If no type is indicated, the type is string."
--
emptyResponse :: L.ByteString
emptyResponse = "<?xml version='1.0' ?>\n<methodResponse><params><param><value/></param></params></methodResponse>"
   -- ds9 7.4b8 fails with "Unrecognized response from server"
-- emptyResponse = "<?xml version='1.0' ?>\n<methodResponse><params><param><value></value></param></params></methodResponse>"
   -- ds9 7.4b8 fails with "Invalid close of value tag"
-- emptyResponse = "<?xml version='1.0' ?>\n<methodResponse><params><param><value><string/></value></param></params></methodResponse>"
   -- ds9 7.4b8 fails with "Invalid close of value tag"
-- emptyResponse = "<?xml version='1.0' ?>\n<methodResponse><params><param><value><string></string></value></param></params></methodResponse>"
   -- ds9 7.4b8 fails with "Invalid close of value tag"

-- emptyResponse = "<?xml version='1.0' ?>\n<methodResponse><params><param><value></value></param></params></methodResponse>"
-- emptyResponse = XI.renderResponse (XI.Return (XI.ValueString "")) -- ValueUnwrapped?
-- emptyResponse = renderSAMPResponse (SAMPReturn "")
   -- the response here is
   -- <methodResponse><params><param><value><string></string></value></param></params></methodResponse>
   -- ds9 7.4b8 fails with "Invalid close of value tag"

-- | Return a SAMP map.
--
--   It appears that ds9 7.3.2 has problems parsing empty
--   XML elements, in that it seems to only accept <a></a>
--   and not <a/>. This is a problem, since the XML writer
--   used here uses the latter form. So, I manually hack
--   the output to use the latter form, for a=struct.
--   This is a band-aid, since there may be other tags that
--   need this change.
--
makeResponse :: [SAMPKeyValue] -> L.ByteString
makeResponse kvs =
    let orig = renderSAMPResponse (SAMPReturn (SAMPMap kvs))
    in replaceEmptyStruct orig
       
replaceEmptyStruct :: L.ByteString -> L.ByteString
replaceEmptyStruct = replace "<struct/>" "<struct></struct>"

replace :: B.ByteString -> B.ByteString -> L.ByteString -> L.ByteString
replace find new orig = L.fromStrict (mconcat (reverse (go origB [])))
    where
      origB = L.toStrict orig
      dropB = B.drop (B.length find)
      go xs ys | B.null xs = ys
               | otherwise = let (as, bs) = B.breakSubstring find xs
                             in if B.null bs
                                then as:ys
                                else go (dropB bs) (new:as:ys)


