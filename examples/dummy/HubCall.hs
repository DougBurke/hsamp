{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr, not tested on Windows (will not work due to POSIX file permissions)
------------------------------------------------------------------------

This is only needed whilst the xmlhack flag is being supported. This
module is just a wrapper around the haxr call.

-}

module HubCall (call, fromSAMPMethodResponse) where

import qualified Data.ByteString.Lazy.Char8 as L

import qualified Network.XmlRpc.Client as XC
import qualified Network.XmlRpc.Internals as XI

import Network.SAMP.Standard (SAMPMethodResponse
                             , renderSAMPResponse)
    
call :: String
     -> String
     -> [XI.Value]
     -> XI.Err IO XI.Value
call = XC.call

-- | This is the default conversion to XML format for a SAMP
--   method response.
fromSAMPMethodResponse :: SAMPMethodResponse -> L.ByteString
fromSAMPMethodResponse = renderSAMPResponse
