{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2015, 2016
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr, not tested on Windows (will not work due to POSIX file permissions)
------------------------------------------------------------------------
-}

{-
Test out a SAMP server; some of this code should probably go into
the hSAMP library.

From http://www.ivoa.net/documents/SAMP/20120411/REC-SAMP-1.3-20120411.html

2.4  The Lifecycle of a Hub

A SAMP hub goes through the following phases:

1 Locate any existing hub by using the appropriate hub discovery mechanism.
Check whether the existing hub is alive.  If so, exit.

2 If no hub is running, or a hub is found but is not functioning,
write/overwrite the hub discovery record and start up.

3 Await client registrations. When a client makes a legal registration, assign
it a public ID, and add the client to the table of registered clients under the
public ID. Broadcast a message announcing the registration of a new client.

4 When a client stores metadata in the hub, broadcast a message announcing the
change and make the metadata available.

5 When a client updates its list of subscribed MTypes, broadcast a message
announcing the change and make the subscription information available

6 When the hub receives a message for relaying, pass it on to appropriate
recipients which are subscribed to the message's MType. Broadcast messages are
sent to all subscribed clients except the sender, messages with a specified
recipient are sent to that recipient if it is subscribed.

7 Await client unregistrations. When a client unregisters, broadcast a message
announcing the unregistration and remove the client from the table of
registered clients.

8 If the hub is unable to communicate with a client, it may unregister it as
described in phase 7.

9 When the hub is about to shutdown, broadcast a message to all subscribed
clients.

10  Delete the hub discovery record.

Phases 3-8 are responses to events which may occur multiple times and in any
order.

The MTypes broadcast by the hub to inform clients of changes in its state are
given in Section 6.4.1.

Readers should note that, given this scheme, race conditions may occur. A
client might for instance try to register with a hub which has just shut down,
or attempt to send to a recipient which has already unregistered. Specific
profiles MAY define best-practice rules in order to best manage these
conditions, but in general clients should be aware that SAMP's lack of
guaranteed message delivery and timing means that unexpected conditions are
possible.

TODO:

  - if the hub is killed then messages about the hub closing down are often
    not sent, because the hub shut down is too quick (i.e. does not wait for
    the messages to be sent).

  - switch to async (if it makes sense; at present most of the concurrent
    calls do not have a useful return value). Here's my original
    todo item:

      could run connections to clients using async, storing the
      thread ids, so that they can be stopped if the client shut
      downs (or hub or ...)

  - add web profile

  - work out what's going on with ds9 connections
    - have sorted out will Bill the 'Parse exception: not enough input'
      that I get from handling responses from ds9 (content-length header
      is being set incorrectly by ds9)
    - todo: validate that ds9 supports <x/> form of tags

  - removal of problematic clients

  - handling of x-samp messages

  - clean up and refactoring, e.g. 
    - replace MType with separate "complete" and "allows wildcard"
      versions

-}

module Main (main) where

import qualified Control.Exception as CE

import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT

-- Hack for ds9 7.3.2 support
import qualified HubCall as HC
    
import qualified Network as N
import qualified Network.XmlRpc.Internals as XI

-- ghc 7.10.2/3 says that this import is redundant, but if
-- it is commented out then fdWrite and closeFd are not defined
import qualified System.Posix.IO as P

import System.Directory (removeFile)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (ownerReadMode, ownerWriteMode)
import System.Posix.Types (Fd)
import System.Random (RandomGen, StdGen, getStdGen, setStdGen)
-- import System.Timeout (timeout)

import Control.Arrow (first)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar,
                                     takeTMVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_, guard, unless, void, when)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (liftIO)

import Data.Bits ((.|.))
import Data.ByteString.Base64.Lazy (decodeLenient)
import Data.Default.Class (def)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Semigroup ((<>))
import Data.Time.Clock (UTCTime, getCurrentTime)
    
-- import Data.Version (showVersion)

import Network.SAMP.Standard (MType, RString
                             , SAMPMapValue
                             , SAMPMapElement
                             , MessageId, toMessageId
                             , MessageTag, toMessageTag
                             , SAMPResponse, SAMPMethodCall(..)
                             , SAMPMethodResponse(..)
                             , SAMPValue(..)
                             , SAMPMessage
                             , SAMPInfo
                             , getSAMPInfoLocation, getSAMPInfoHubURL
                             , ClientName, ClientSecret
                             , HubSecret
                             , toClientName, fromClientName
                             , toClientSecret
                             , toHubSecret, fromHubSecret
                             , fromSValue, toSValue
                             , fromRString, toRString
                             , fromMType, fromMTypeRS
                             , toMType, toMTypeE, isMTWildCard
                             , randomAlphaNumRString
                             , toSAMPResponse
                             , toSAMPResponseError
                             , toSAMPMessageMT
                             , getSAMPMessageType
                             , getSAMPMessageParams
                             , getSAMPMessageExtra
                             , getKey
                             , parseSAMPCall
                             , handleError
                             , pingHubE
                             , runE)
    
import Network.SAMP.Standard.Setup (getHubInfoE, sampLockFileE)
-- import Network.SAMP.Standard.Server.Scotty (runServer)

import Network.Socket (Socket, socketPort)
    
import Network.HTTP.Types (status400)
import Network.URI (URI(..), parseURI)

import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (Destination(..), destination,
                                             -- logStdoutDev,
                                             mkRequestLogger)

import System.Log.Logger (Priority(DEBUG, INFO, NOTICE, WARNING),
                          debugM, infoM, noticeM, warningM,
                          setLevel, updateGlobalLogger)
import System.Log.FastLogger (fromLogStr)
import System.Posix.IO (createFile)
    
import Web.Scotty (ActionM
                  , body, header, middleware, get, post
                  , json, raw
                  , setHeader, scottySocket, status, text)

import Utils (getSocket)

-- import Paths_hsamp (version)

-- the name of the SAMP logging instance
cLogger :: String
cLogger = "SAMP.StandardProfile.Hub"

-- log the message to the SAMP logger
--
dbg, info, warn :: MonadIO m => String -> m ()
dbg = liftIO . dbgIO
info = liftIO . infoIO
warn = liftIO . warnIO

{- currently unused       
notice :: String -> ActionM ()
notice = liftIO . noticeIO
-}

dbgIO, infoIO, noticeIO, warnIO :: String -> IO ()
dbgIO = debugM cLogger . ("DBG " ++)
infoIO = infoM cLogger . ("INFO " ++)
noticeIO = noticeM cLogger . ("NOTICE " ++)
warnIO = warningM cLogger . ("WARNING " ++)
           
main :: IO ()
main = do
    args <- getArgs
    updateGlobalLogger "SAMP" (setLevel WARNING)
    _ <- case args of
        [] -> return ()
        ["--debug"] -> updateGlobalLogger "SAMP" (setLevel DEBUG)
        ["--info"] -> updateGlobalLogger "SAMP" (setLevel INFO)
        ["--notice"] -> updateGlobalLogger "SAMP" (setLevel NOTICE)
        _ -> do
            pName <- getProgName
            hPutStrLn stderr ("Usage: " ++ pName ++ " --debug")
            exitFailure

    runHub
    exitSuccess

-- | Return the location of the lock file. If the file exists then
--   the hub is pinged once, and if this fails, it is assumed that
--   the file is "stale" and over-written.
--
--   This has not been tested on Windows so will probably fail there.
--   It does not use Haskell's System.Directory.getHomeDirectory since
--   this is unlikely to give the correct location on Windows.
--
--   This follows SAMP version 1.2 for location of the lock file.
--
--   At present this fails if the URI is not a file-based one.
--
--   This should probably use 'withSAMP' (although it is a no-op
--   with ghc 7.10).
--
getLockFile :: IO FilePath
getLockFile = do
  -- Would be nice to identify problematic hubs (e.g. if the
  -- hub contents are invalid), but I do not want to try and
  -- distinguish between the form of error (file does not exist
  -- versus missing key) so just return Nothing.
  --
  uri <- runE sampLockFileE
  when (uriScheme uri /= "file:")
           (fail ("The hub file location must be a file, not "
                  ++ show uri))

  mSampInfo <- handleError (const (return Nothing))
               (Just <$> getHubInfoE uri)
               
  case mSampInfo of
    Just sampInfo -> checkHubRunning sampInfo
                     >> return (uriPath (getSAMPInfoLocation sampInfo))
    _ -> return (uriPath uri)

         
-- | Verifies that the hub file reflects a running Hub - calling
--   'exitFailure' if it is, otherwise returns to the caller.
--
--   The check is done by calling samp.hub.ping on the hub; if
--   it responds then error out.
--
checkHubRunning :: SAMPInfo -> IO ()
checkHubRunning si = do
  let uri = getSAMPInfoHubURL si
      hubName = uriPath (getSAMPInfoLocation si) -- assumed to be a file
  running <- handleError (const (return Nothing)) (Just <$> pingHubE uri)
  case running of
    Just _ -> do
      hPutStrLn stderr ("ERROR: Is a SAMP hub already running? " ++
                        hubName ++ " already exists.")
      exitFailure
      
    Nothing -> do
      putStrLn ("The hub with SAMP lock file " ++ hubName ++
                " has not responded.")
      putStrLn " ... so removing."
      -- TODO: should remove it in case it is write-protected
      return ()
              

-- | Create the hub file. The file is set to have no permissions
--   in the group or owner settings (this makes the routine non-portable).
--
--   It is a bit ugly; is there an easier way to do this?
writeLockFile :: FilePath -> String -> HubSecret -> IO ()
writeLockFile name url secret = do
    let cts = [ "# TEST SAMP hub, using the Haskell SAMP library"
              , "samp.secret=" ++ fromRString (fromHubSecret secret)
              , "samp.hub.xmlrpc.url=" ++ url
              , "samp.profile.version=1.2" ]
        str = unlines cts

        flags = ownerReadMode .|. ownerWriteMode
        act fd = void (P.fdWrite fd str)

        cleanup :: Fd -> CE.SomeException -> IO ()
        cleanup fd e = do
          P.closeFd fd
          removeFile name
          CE.throwIO e
                    
    fd <- P.createFile name flags
    act fd `CE.catch` cleanup fd
    
-- | Create a secret string. Although any RChar is technically permitted
--   (the SAMP standard v1.3 does not mention the form of the string),
--   it is probably best to restrict the characters to alpha-numeric
--   ones, to avoid unnescessary confusion (e.g. spaces or characters
--   that need extra conversion to be sent over the wire).
--
makeSecret :: RandomGen g => g -> (RString, g)
makeSecret = randomAlphaNumRString 16

-- | Create a new message identifier, including updating the
--   hub with the information about it.
--
--   TODO: Note that, at present, there is a lot of repeated work
--   here and in the caller (i.e. checking that the receiver is
--   known). This needs to be consolidated once the sendToAll processing
--   has been written.
--
newMessageId ::
    HubInfo
    -> ClientSecret
    -- ^ the client which will be sent the message Id
    -> (MessageTag, ClientData, Maybe (TMVar SAMPMethodResponse))
    -- ^ the source of the message
    -> IO (Either String MessageId)
    -- ^ in case the client can not be found
newMessageId hi secret (tag, sender, mrsp) = do
  cTime <- getCurrentTime
  changeHubWithClientE hi secret $ \ohub orec -> 
    let oclientmap = hiClients ohub
        oflight = cdInFlight orec
        onum = cdNextInFlight orec
        nnum = succ onum

        ifd = IFD { ifdSecret = cdSecret sender
                    -- if get to here the sender must have a callback
                  , ifdUrl = fromJust (cdCallback sender)
                  , ifdTag = tag
                  , idfMRsp = mrsp
                  , ifdTime = cTime }
        
        -- Inserting a prefix to ensure no collision is
        -- rather OTT here, given that the likelihood of
        -- collision is small. However, it may make manual
        -- tracing of message flow a bit easier.
        (s, ngen) = makeSecret (hiRandomGen ohub)

        rstr = fromJust (toRString (show onum))
        mid = toMessageId ("mid" <> rstr <> ":" <> s)
              
        nflight = M.insert mid ifd oflight
        nrec = orec { cdInFlight = nflight
                    , cdNextInFlight = nnum }
        nclMap = M.adjust (const nrec) secret oclientmap
        nhub = ohub { hiClients = nclMap
                    , hiRandomGen = ngen }

        sact = do
          dbgIO ("Inserted messageId=" ++ show mid ++ " for receiver=" ++
                 show (cdName orec))
          nclMap `seq` nhub `seq` nflight `seq` ifd `seq`
            mid `seq` nnum `seq` return ()

    in (nhub, mid, sact)
            

removeMessageId ::
    HubInfo
    -> MessageId     -- ^ message to remove
    -> ClientSecret  -- ^ secret of client that uses the message
                     --   (i.e. the one that uses this id)
    -> IO ()
removeMessageId hi mid secret =
    -- could check the return value to display a warning about
    -- "invalid" secret
    void $ changeHubWithClientE hi secret $ \ohub oclient -> 
      let oclMap = hiClients ohub
          oflMap = cdInFlight oclient
          nflMap = M.delete mid oflMap
          nclient = oclient { cdInFlight = nflMap }
          nclMap = M.adjust (const nclient) secret oclMap
          nhub = ohub { hiClients = nclMap }
                   
          sact = do
            dbgIO ("Removing message id " ++ show mid ++ " client " ++
                   show (cdName oclient))
            nhub `seq` nclMap `seq` nclient `seq` nclMap `seq` return ()

      in (nhub, (), sact)

               
setupHubFile :: IO (Socket, FilePath, HubSecret)
setupHubFile = do
    lockfile <- getLockFile
    putStrLn ("Using lock file: " ++ lockfile)

    -- what socket is going to be used?
    sock <- getSocket
    port <- socketPort sock
    putStrLn ("Using port " ++ show port ++ " for the SAMP hub.")

    gen <- getStdGen
    let (secret, gen') = makeSecret gen
        hubSecret = toHubSecret secret
    setStdGen gen'

    let xmlrpc = addXMLRPCEndPoint (portToURL port)
    writeLockFile lockfile xmlrpc hubSecret
    return (sock, lockfile, hubSecret)

portToURL :: N.PortNumber -> String
portToURL p = "http://127.0.0.1:" ++ show p ++ "/"

-- | This asssumes a valid URL on input.
addXMLRPCEndPoint :: String -> String
addXMLRPCEndPoint u = u ++ "xmlrpc/"
                     
getHubURL :: Socket -> IO String
getHubURL sock = portToURL <$> socketPort sock

         
runHub :: IO ()
runHub = do
    (socket, lockfile, secret) <- setupHubFile

    -- perhaps should be done in setupHubFile?
    gen <- getStdGen
    huburl <- getHubURL socket
    hi <- newHubInfo huburl gen secret

    let cleanup :: CE.SomeException -> IO ()
        cleanup e = do
           -- not clear when to bother with this (i.e. what errors
           -- mean it isn't worth sending the shutdown signal); for
           -- now try in all cases
           broadcastShutDown hi
           N.sClose socket
           noticeIO ("Removing lockfile: " ++ lockfile)
           removeFile lockfile
           CE.throwIO e
    createHub socket hi secret `CE.catch` cleanup

    -- is it possible to get here once the hub is running?
    N.sClose socket
    removeFile lockfile

-- This currently will wait for a response from a client to the
-- shutdown message; should this be changed as it should just be
-- send and forget?
--
-- This sends a samp.hub.event.unregister and then shutdown,
-- but doesn't actually do anything with the unregister call
-- (i.e. does not unregister the clients). Should it be sending
-- this; I did so because I saw this snooping the traffic between
-- JSAMP and ds9, but have since been unable to replicate this.
--
-- This catches any error during sending, so that messges can be
-- sent to all clients.
--
broadcastShutDown :: HubInfo -> IO ()
broadcastShutDown hi = do
  noticeIO "\n"
  noticeIO "Hub is shutting down."
  let name = toClientName (hiName (hiReader hi))
  hub <- atomically (readTVar (hiState hi))
  forM_ (M.elems (hiClients hub))
        (broadcastRemovedClient hi . cdName)
  broadcastMType hi name
    (toSAMPMessageMT "samp.hub.event.shutdown" M.empty M.empty)


-- TODO: should some of these be left in STM rather than converted
--       to IO?

-- | Run an action with a copy of the Hub's state.
--   See 'changeHub' for an alternative.
--
withHub ::
    HubInfo
    -> (HubInfoState -> IO a)
    -> IO a
withHub hi act = atomically (readTVar (hiState hi)) >>= act

withHubP ::
    HubInfo
    -> (HubInfoState -> a)
    -> IO a
withHubP hi act = do
  hub <- atomically (readTVar (hiState hi))
  return (act hub)


{-
TODO: make sure that the code run witihn the changeHub calls is minimal,
offloading as much as possible to the action to run afterwards
-}

-- | Run an action and change the hub state.
--
changeHub ::
    HubInfo
    -> (HubInfoState -> (HubInfoState, a))
       -- ^ The action is performed with a @modifyTVar'@ call.
       --   The return value is the new hub state.
    -> IO a
changeHub hi act =
  let go = do
        let tvar = hiState hi
        ostate <- readTVar tvar
        let (nstate, ans) = act ostate
        writeTVar tvar nstate
        return ans
  in atomically go

-- | Run an action with a copy of the Hub's state. See
--   'changeHubWithClient' for an alternative.
--
--   This is similar to 'withHub' but ensures that the client secret
--   is valid. Ideally it would not return a 'ByteString' but
--   an XML-RPC return type, but for now - to support ds9 7.3.2 -
--   a bytestring is used (to save a little bit of time checking the
--   response to see if it needs to be set to the hard-coded
--   emptyResponse value).
--
withClient ::
    HubInfo
    -> ClientSecret
    -> (HubInfoState -> ClientData -> IO SAMPMethodResponse)
       -- ^ Action to perform if the secret matches a client. The
       --   client data represents the client that has initiatied the
       --   action, and the HubInfoState is valid at the time the
       --   message is processed (but may become out of date at any
       --   time)
    -> IO SAMPMethodResponse
withClient hi secret act =
    withHub hi $ \hub ->
      case M.lookup secret (hiClients hub) of
        Just sender -> act hub sender
        _ -> do
          info ("Unable to find a client with secret=" ++ show secret)
          return (rawError "Invalid client id")


-- | Run an action - checking that the client is valid -
--   and copy the hub state.
--
changeHubWithClient ::
    HubInfo
    -> ClientSecret
    -> (HubInfoState -> ClientData -> (HubInfoState, SAMPMethodResponse, IO ()))
       -- ^ The return value is the new hub state, the return
       --   value, and an IO action to run after setting the
       --   new hub state (e.g. any broadcast message to be sent or
       --   code to ensure the new state is evaluated).
    -> IO SAMPMethodResponse
changeHubWithClient hi secret act = do
  mrsp <- changeHubWithClientE hi secret act
  case mrsp of
    Right rsp -> return rsp
    Left emsg -> reportError emsg


-- | Run an action - checking that the client is valid *and* callable -
--   and copy the hub state.
--
changeHubWithCallableClient ::
    HubInfo
    -> ClientSecret
    -> (HubInfoState -> ClientData -> (HubInfoState, SAMPMethodResponse, IO ()))
       -- ^ The return value is the new hub state, the return
       --   value, and an IO action to run after setting the
       --   new hub state (e.g. any broadcast message to be sent or
       --   code to ensure the new state is evaluated).
    -> IO SAMPMethodResponse
changeHubWithCallableClient hi secret act = do
  mrsp <- changeHubWithCallableClientE hi secret act
  case mrsp of
    Right rsp -> return rsp
    Left emsg -> reportError emsg


changeHubWithClientE ::
    HubInfo
    -> ClientSecret
    -> (HubInfoState -> ClientData -> (HubInfoState, a, IO ()))
       -- ^ The return value is the new hub state, the return
       --   value, and an IO action to run after setting the
       --   new hub state (e.g. any broadcast message to be sent or
       --   code to ensure the new state is evaluated).
    -> IO (Either String a)
changeHubWithClientE hi secret act = do
  let tvar = hiState hi
      doit ohub = do
        let clMap = hiClients ohub
        case M.lookup secret clMap of
          Just client -> 
            let (nhub, rsp, sact) = act ohub client
            in (nhub, (Right rsp, sact))
                
          _ -> let emsg = "Invalid client secret"
               in (ohub, (Left emsg, return ()))

      go = do
        ostate <- readTVar tvar
        let (nstate, ans) = doit ostate
        writeTVar tvar nstate
        return ans

  (rsp, sact) <- atomically go
  sact
  return rsp

         
changeHubWithCallableClientE ::
    HubInfo
    -> ClientSecret
    -> (HubInfoState -> ClientData -> (HubInfoState, SAMPMethodResponse, IO ()))
       -- ^ The return value is the new hub state, the return
       --   value, and an IO action to run after setting the
       --   new hub state (e.g. any broadcast message to be sent or
       --   code to ensure the new state is evaluated).
    -> IO (Either String SAMPMethodResponse)
changeHubWithCallableClientE hi secret act = 
  changeHubWithClientE hi secret $ \ohub client -> do
    if isJust (cdCallback client)
      then act ohub client
      else let emsg = "Client " ++ show (cdName client) ++ " " ++
                      getFullName client ++
                      " is not callable"

           in (ohub, rawError emsg, return ())

-- | Process a message from a client that you can send a message to
--   (a restricted variant of 'withClient')
--
withCallableClient ::
    HubInfo
    -> ClientSecret
    -> (HubInfoState -> ClientData -> IO SAMPMethodResponse)
       -- ^ Action to perform if the secret matches a client and the client is
       --   callable. The client data represents the client that has initiatied
       --   the action, and the HubInfoState is valid at the time the message
       --   is processed (but may become out of date at any time)
    -> IO SAMPMethodResponse
withCallableClient hi secret act =
    withClient hi secret $ \hub sender ->
        if isJust (cdCallback sender)
          then act hub sender
          else let emsg = "Client " ++ show (cdName sender) ++ " " ++
                          getFullName sender ++
                          " is not callable"
                          
               in reportError emsg
                
         
-- pulling out common code for handling notify messages;
-- can perhaps be made local again once this is finished.
--

notifyClient ::
    RString
    -> SAMPMessage
    -> ClientData
    -> IO SAMPMethodResponse
notifyClient sendName msg receiver = do
  let mtype = getSAMPMessageType msg
      recName = cdName receiver
  info ("Sending " ++ fromMType mtype ++ " from " ++
        fromRString sendName ++ " to " ++ show recName)
  forkCall (notifyRecipient sendName msg receiver)
  return emptyResponse

-- The receiver is the hub, so do not need to send a message,
-- but do we need to do something? At this point we know the
-- hub is subscribed to the message
--
notifyHub ::
    RString
    -> SAMPMessage
    -> IO SAMPMethodResponse
notifyHub sendName msg = do
  let mtype = getSAMPMessageType msg
  info ("Hub has been notified about " ++ show mtype ++ " from " ++
        show sendName)
  return emptyResponse


findClientsToNotify ::
    HubInfoReader
    -> HubInfoState
    -> SAMPMessage
    -> ClientData
    -- ^ The client sending the message
    -> (Bool, [ClientData])
    -- ^ The first element indicates whether the hub is to be
    --   notified, the second the list of clients to notify
findClientsToNotify hir hub msg sender =
    let mtype = getSAMPMessageType msg
        clMap = hiClients hub
        sendName = cdName sender
               
        hubMatch = isHubSubscribed hir mtype sendName
        receivers = findOtherSubscribedClients clMap sendName mtype
                  
    in (hubMatch, receivers)


findClientToNotify ::
    HubInfo
    -> ClientName
    -> SAMPMessage
    -> HubInfoState
    -> ClientData
    -> IO SAMPMethodResponse
findClientToNotify hi recName msg hub sender =
    let sendName = fromClientName (cdName sender)
        hir = hiReader hi
        mtype = getSAMPMessageType msg

        (mHubMatch, receivers) = findClientsToNotify hir hub msg sender

        find cd = cdName cd == recName
        mreceivers = filter find receivers

        hubMatch = mHubMatch && hiName hir == fromClientName recName
    
        noReceiver = 
          let emsg = "The receiver is not callable or not subscribed to " ++
                     fromMType mtype
          in reportError emsg
                 
    in if hubMatch
       then notifyHub sendName msg
       else case mreceivers of
               (receiver:_) -> notifyClient sendName msg receiver
               [] -> noReceiver


-- | A client wants to send a message to another client via the notify
--   system.
--
--   TODO:
--     do we want to treat the hub as a normal client - i.e. send it
--     a message - or short-circuit this here? Which is easier?
--     AT THE MOMENT WE DROP THE MESSAGE (ie do nothing with it) OTHER
--     THAN RESPOND TO IT.
--
notify ::
    HubInfo
    -> ClientSecret   -- ^ the client sending the message
    -> ClientName     -- ^ the client to send the message to
    -> SAMPMessage    -- ^ the message to send
    -> IO (String, SAMPMethodResponse)
notify hi secret recName msg = do
  rsp <- withClient hi secret (findClientToNotify hi recName msg)
  return ("notify", rsp)


-- | A client wants to send a message to all the interested clients
--   via the notify system.
--
notifyAll ::
    HubInfo
    -> ClientSecret   -- ^ the client sending the message
    -> SAMPMessage    -- ^ the message to send
    -> IO (String, SAMPMethodResponse)
notifyAll hi secret msg = do
  rsp <- withClient hi secret $ \hub sender -> do
      let sendName = fromClientName (cdName sender)
          hir = hiReader hi
          (hubMatch, receivers) = findClientsToNotify hir hub msg sender
                                  
      forM_ receivers (notifyClient sendName msg)
      when hubMatch (void (notifyHub sendName msg))
                   
      let rNames = map (toSValue . cdName) receivers
          hName = SAMPString (hiName hir)
          names = if hubMatch then hName : rNames else rNames
          rval = SAMPReturn (SAMPList names)
      return rval

  return ("notifyAll", rsp)


findOtherSubscribedClients ::
    ClientMap
    -> ClientName -- ^ client to exclude from the search
    -> MType
    -> [ClientData]
findOtherSubscribedClients clMap clName mtype =
    let find = isClientSubscribed clName mtype
    in M.elems (M.filter find clMap)


isClientSubscribed ::
  ClientName
  -- ^ The client sending the message (i.e. not included in the check)
  -> MType
  -> ClientData
  -> Bool
isClientSubscribed sendName mtype cd =
    cdName cd /= sendName &&
    isJust (cdCallback cd) &&
    memberSubMap mtype (cdSubscribed cd)


-- | Return the client if it is callable and subscribed to the message.
--
findSubscribedClientE ::
    ClientMap
    -> MType
    -> ClientName
    -> Either String ClientData
findSubscribedClientE clMap msg clName =
    let isClient cd = cdName cd == clName
        nameStr = show clName
    
    in case filter isClient (M.elems clMap) of
         (cd:_) -> if isJust (cdCallback cd)
                   then if memberSubMap msg (cdSubscribed cd)
                        then Right cd
                        else Left ("Client " ++ nameStr ++ " " ++
                                   getFullName cd ++
                                   " is not " ++
                                   "subscribed to " ++ show msg)
                   else Left ("Client " ++ nameStr ++ " " ++
                              getFullName cd ++
                              " is not callable")
         _ -> Left ("There is no client called " ++ nameStr)

       
-- Check if the hub is subscribed to the message and is not the
-- sender
isHubSubscribed :: HubInfoReader -> MType -> ClientName -> Bool
isHubSubscribed hir mtype sendName =
    hiName hir /= fromClientName sendName &&
    memberSubMap mtype (hiSubscribed hir)

           
-- | A client wants to send a message to another client via the
--   call system.
--
--   TODO: does this need to cope with the receiver being the hub?
--
sendToClient ::
    HubInfo
    -> ClientSecret   -- ^ the client sending the message
    -> ClientName     -- ^ the client to send the message to
    -> MessageTag
    -> SAMPMessage    -- ^ the message to send
    -> IO (String, SAMPMethodResponse)
sendToClient hi secret recName tag msg = do

  rsp <- withCallableClient hi secret $ \hub sender -> do
      let clMap = hiClients hub
          mtype = getSAMPMessageType msg
              
      case findSubscribedClientE clMap mtype recName of
        Right rcd -> sendToC hi sender tag msg rcd
        Left emsg -> reportError emsg
                    
  return ("call", rsp)


{-

client sends hub the call message, a tag, recipient and the message 
   (type+params)

   hub calls the recipient, using a message id, and waits for the response
   then sends the response back to the client
 

the above code calls the recipient and adds a note to the hub to
say that the message is in flight, and so responding to the recipient
(to send back to the client) is not done here.

Perhaps, instead, the call to the recipient and sending back to the
client, should be done in a separate thread? Probably not, since
the sequence is

  client    -> hub         call(msg-tag)
  hub       -> recipient   receiveCall(msg-id)
  recipient -> hub         reply(msg-id)
  hub       -> client      receiveResponse(msg-tag)

so it's not obvious to me that spawing a thread to deal with
this is feasible (since it involves the scotty handler).

For now gone with setting up a MVar which the hub will fill
once it has received the message (for the callAndWait case).
-}
          

-- | A client wants to send a message to all clients via the
--   call system.
--
sendToAll ::
    HubInfo
    -> ClientSecret   -- ^ the client sending the message
    -> MessageTag
    -> SAMPMessage    -- ^ the message to send
    -> IO (String, SAMPMethodResponse)
sendToAll hi secret tag msg = do

  -- get the time before waiting for the hub lock (at present the time
  -- is not used, so it does not really matter)
  cTime <- getCurrentTime

  {-
  could use changeHubWithClientE so that can return a non-SAMPResponse,
  which could mean less work in sendToAll2/no need to send in hi
  -}
  let act = sendToAll2 cTime hi tag msg
  rsp <- changeHubWithClient hi secret act
  return ("callAll", rsp)


-- The idea is to iterate through the hub, changing the client data
-- of the subscribed clients, and build up the IO actions to notify
-- these clients. This action is then run *outside* the STM
-- transaction.
--
sendToAll2 ::
  UTCTime
  -> HubInfo -- wanted to send in HubInfoReader, but need hub
  -> MessageTag
  -> SAMPMessage  -- ^ The message to send
  -> HubInfoState
  -> ClientData -- ^ The sender of the message
  -> (HubInfoState, SAMPMethodResponse, IO ())
sendToAll2 cTime hi tag msg ohub sender =
  let hir = hiReader hi
      mtype = getSAMPMessageType msg
      sendName = cdName sender

      ifd = IFD { ifdSecret = cdSecret sender
                  -- if get to here the sender must have a callback
                , ifdUrl = fromJust (cdCallback sender)
                , ifdTag = tag
                , idfMRsp = Nothing
                , ifdTime = cTime }

      isHub = isHubSubscribed hir mtype sendName

      oClMap = hiClients ohub

      -- Update the client, if necessary
      --
      update oacc@(ogen, orvals, oacts) ocd =
        if isClientSubscribed sendName mtype ocd
        then
          let (s, ngen) = makeSecret ogen
  
              oflight = cdInFlight ocd
              onum = cdNextInFlight ocd
              
              nnum = succ onum
              
              rstr = fromJust (toRString (show onum))
              mid = toMessageId ("mid" <> rstr <> ":" <> s)
  
              nflight = M.insert mid ifd oflight
              ncd = ocd { cdInFlight = nflight
                        , cdNextInFlight = nnum }
  
              act = do
                mid `seq` nnum `seq` nflight `seq` ncd `seq` return ()
                dbgIO ("Inserted messageId=" ++ show mid ++
                       " for receiver=" ++ show (cdName ocd))
                forkCall (clientReceiveCall sendName ncd mid msg)
  
              nrvals = (fromClientName (cdName ocd), toSValue mid) : orvals
              nacts = oacts >> act
              
          in ((ngen, nrvals, nacts), ncd)
        else (oacc, ocd)
             
      -- mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
      (acc1, nClMap) = M.mapAccum update
                       (hiRandomGen ohub, [], return ())
                       oClMap
      (ngen1, rvals1, acts1) = acc1

      (hgen, hvals, hacts) =
        if isHub
        then
          let (s, gen) = makeSecret ngen1
              -- unlike the clients, do not include a counter here
              mid = toMessageId s

              hval = (hiName hir, toSValue mid)

              hact = do
                mid `seq` hgen `seq` return () -- how useful is this?
                void (forkIO (sendToHub hi tag sender msg))
              
          in (gen, hval : rvals1, hact >> acts1)
             
        else acc1

      nhub = ohub { hiClients = nClMap
                  , hiRandomGen = hgen
                  }

  in (nhub, SAMPReturn (SAMPMap (M.fromList hvals)), hacts)


ignoreError :: String -> CE.SomeException -> IO ()
ignoreError lbl e = do
  hPutStrLn stderr ("*** Error " ++ show lbl)
  hPutStrLn stderr ("*** " ++ show e)


ignoreNotifyError :: ClientName -> String -> MType -> CE.SomeException -> IO ()
ignoreNotifyError name fname mtype =
  let emsg = "notifying client " ++ show name ++
             (if null fname then "" else " " ++ fname ++ " ") ++
             " about " ++ show mtype
  in ignoreError emsg
  
    
sendReceiveResponse ::
    String
    -> ClientSecret
    -> ClientName
    -> MessageTag
    -> SAMPResponse
    -> IO ()
sendReceiveResponse url secret name tag replyData = do
  let arglist = [ toSValue secret
                , toSValue name
                , toSValue tag
                , toSValue replyData ]
      nargs = map XI.toValue arglist

      act = void (HC.call url "samp.client.receiveResponse" nargs)
      hdl e = dbgIO ("Error in sending reply to the original client at " ++
                     url ++ " - " ++ e)

  dbgIO ("Sending receiveResponse from " ++ show name)
  handleError hdl act `CE.catch` ignoreError "notifying original client"


-- | The hub has been sent a message as a SAMP client. It is
--   therefore one of the messages that the hub is subscribed
--   to.
--
sendToHub ::
    HubInfo
    -> MessageTag
    -> ClientData
    -- ^ The client sending the message (it is possible that this client
    --   has since disconnected).
    -> SAMPMessage
    -> IO ()
sendToHub hi mtag sender msg = do
  let mtype = getSAMPMessageType msg
      sendName = cdName sender
      url = fromJust (cdCallback sender)
      secret = cdSecret sender
      hubName = toClientName (hiName (hiReader hi))
      
  dbgIO ("Hub sent " ++ fromMType mtype ++ " by " ++ show sendName)
  replyData <- hubProcessMessage hi msg
  sendReceiveResponse url secret hubName mtag replyData


-- | The hub has been sent a message it is subscribed to.
--
hubProcessMessage ::
    HubInfo
    -> SAMPMessage
    -> IO SAMPResponse
hubProcessMessage hi msg = do
  let funcs = hiHandlers (hiReader hi)
      emsg = "Internal error: hub is not subscribed to " <> fromMTypeRS mtype
      mtype = getSAMPMessageType msg
      otherArgs = getSAMPMessageExtra msg
  case lookup mtype funcs of
    Just f -> f hi msg
    _ -> return (toSAMPResponseError emsg M.empty otherArgs)


-- | Used when a return is needed, but it carries no extra information.
emptyResponse :: SAMPMethodResponse
emptyResponse = SAMPReturn (SAMPString "")

                
-- | Process the samp.app.ping message sent to the hub
hubSentPing :: HubHandlerFunc
hubSentPing _ msg = do
  let args = getSAMPMessageParams msg
      extra = getSAMPMessageExtra msg
  infoIO "Hub has been sent samp.app.ping"
  infoIO ("params = " ++ show args)
  infoIO (" extra = " ++ show extra)
  return (toSAMPResponse M.empty M.empty) -- return extra?


-- | Process the [x-]samp.query.by-meta message sent to the hub.
hubSentQueryByMeta :: HubHandlerFunc                 
hubSentQueryByMeta hi msg = do
  let params = getSAMPMessageParams msg
      mtype = getSAMPMessageType msg
      extra = getSAMPMessageExtra msg

      act :: XI.Err (Either RString) (RString, SAMPValue)
      act = do
        key <- getKey "key" params
        val <- getKey "value" params
        return (key, val)

      -- technically the error message could be an invalid RString,
      -- but that should only be due to a bug in this code
      conv str = case toRString str of
                   Just r -> Left r
                   _ -> Left ("Error processing key or value parameters of "
                              <> fromMTypeRS mtype)
                        
      evals = case toRString (show params) of
                Just pstr -> M.singleton "samp.debugtxt" (toSValue pstr)
                _ -> M.empty
      errRsp emsg = toSAMPResponseError emsg evals M.empty

      getMatches kwant vwant =
        withHubP hi $ \hub ->
        let clMap = hiClients hub
            -- the check is case sensitive
            check mdata = isJust (do
                kval <- M.lookup kwant mdata
                guard (vwant == kval))
                           
            find = check . cdMetadata
            clients = map (fromClientName . cdName)
                      (M.elems (M.filter find clMap))

            hir = hiReader hi
            out = if check (hiMetadata hir)
                  then hiName hir : clients
                  else clients

            pvals = M.singleton "ids" (toSValue out)
        in toSAMPResponse pvals extra

  infoIO ("Hub has been sent " ++ show mtype)
  case handleError conv act of
    Right (key, value) -> getMatches key value
    Left emsg -> return (errRsp emsg)

                 
toDuration :: Int -> Integer
toDuration = toInteger . (1000000 *)

sleep :: Integer -> IO ()
sleep t | t <= 0    = return ()
        | otherwise =
            let sleepMax = maxBound :: Int
                (niter, left) = t `quotRem` toInteger sleepMax
                go n | n <= 0    = threadDelay (fromInteger left)
                     | otherwise = threadDelay sleepMax >> go (n-1)
            in go niter


-- TODO: need to clean up sendToCW/sendToC

sendToCW ::
    HubInfo
    -> ClientData
    -> Int
    -> SAMPMessage
    -> ClientData
    -> IO SAMPMethodResponse
sendToCW hi sender waitTime msg receiver = do
  rspvar <- newEmptyTMVarIO
  let arg = (dummyTag, sender, Just rspvar)
      rSecret = cdSecret receiver
      recName = cdName receiver
      mtype = getSAMPMessageType msg
      
  emid <- newMessageId hi rSecret arg
  case emid of
    Right mid -> do
      let sendName = cdName sender
                     
      dbg ("Sending " ++ fromMType mtype ++ " from " ++
           show sendName ++ " to " ++ show recName)

      -- Note: the timer starts at receipt of the message
      --       to the client, not from when the 
      --       samp.client.receiveCall is made
      cid <- forkIO (clientReceiveCall sendName receiver mid msg)

      let tVal = toDuration waitTime
          errorRsp = rawError ("Timeout: call took longer than " ++
                               show waitTime ++ " sec.")

      -- Is there a better way to time out than this?
      when (tVal > 0) (
          forkCall (sleep tVal
                    >> atomically (putTMVar rspvar errorRsp)
                    >> CE.throwTo cid CE.ThreadKilled
                    >> removeMessageId hi mid rSecret)
          )

      atomically (takeTMVar rspvar)

    Left _ -> do
      dbg "*** Looks like the receiver has disappeared ***"
      return (rawError "receiver is no longer available")


sendToC ::
    HubInfo
    -> ClientData
    -> MessageTag
    -> SAMPMessage
    -> ClientData
    -> IO SAMPMethodResponse
sendToC hi sender tag msg receiver = do
    let rSecret = cdSecret receiver
        recName = cdName receiver
        sendName = cdName sender
        mtype = getSAMPMessageType msg
        
    emid <- newMessageId hi rSecret (tag,sender,Nothing)
    case emid of
      Right mid -> do
        info ("Sending " ++ fromMType mtype ++ " from " ++
              show sendName ++ " to " ++ show recName)
        forkCall (clientReceiveCall sendName receiver mid msg)
        return (SAMPReturn (toSValue mid))
  
      Left _ -> do
        info "*** Looks like the receiver has disappeared ***"
        return (rawError "receiver is no longer available")



-- | A client wants to send a message to another client via the
--   call system, waiting for the response.
--
-- TODO: need to cope with the receiver being the hub?
--
callAndWait ::
    HubInfo
    -> ClientSecret   -- ^ the client sending the message
    -> ClientName     -- ^ the client to send the message to
    -> Int            -- ^ the timeout (in seconds)
    -> SAMPMessage    -- ^ the message to send
    -> IO (String, SAMPMethodResponse)
callAndWait hi secret recName waitTime msg = do

  -- NOTE: for callAndWait the client does not need to be callable!
  rsp <- withClient hi secret $ \hub sender -> do
                                               
      -- lots of repeated code with sendToClient
      let mtype = getSAMPMessageType msg

          sendHub = do
            -- no timeout for hub connections!
            replyData <- hubProcessMessage hi msg
            return (SAMPReturn (toSValue replyData))
      
          clMap = hiClients hub
          mRec = findSubscribedClientE clMap mtype recName
      
      dbg ("Processing callAndWait from " ++ show (cdName sender) ++
           "to " ++ show recName)
    
      if fromClientName recName == hiName (hiReader hi)
        then sendHub
        else case mRec of
          Right rcd -> sendToCW hi sender waitTime msg rcd
          Left emsg -> reportError emsg
                   
  return ("callAndWait", rsp)


-- | A client is replying to a message from another client.
--
--   TODO: error out if the client sending the message is not callable.
--
reply ::
    HubInfo
    -> ClientSecret   -- ^ the client sending the message
    -> MessageId
    -> SAMPResponse    -- ^ response to the message
    -> IO (String, SAMPMethodResponse)
reply hi secret msgId replyData = do

  rsp <- withClient hi secret $ \hub _ -> do
                                    
      -- lots of repeated code
  
      let clMap = hiClients hub

          -- check for message tag:
          -- If there is an id in the map, then at present the
          -- client should be callable (since do not allow the
          -- callback field to be unset), but enforce the
          -- check.
          --
          find cd = isJust (cdCallback cd) && M.member msgId (cdInFlight cd)
          recMap = M.filter find clMap

      -- TODO: could use the second argument to the
      -- anonymous function (currently set to _) to get at info
      -- that is in the InFlightData item. What would this
      -- improve or make more complicated?
      --
      -- assume only one match so just use the first one
      case M.elems recMap of
        (cd:_) -> do
            let Just ifd = M.lookup msgId (cdInFlight cd)
            dbg ("Replying to client with secret=" ++
                 show (ifdSecret ifd))
            forkCall (replyToOrigin (cdName cd) ifd replyData)
            removeMessageId hi msgId (cdSecret cd)
            return emptyResponse
                  
        _ -> do
          -- is this considered an error?
          dbg ("No recepient for message-id: " ++ show msgId)
          return (rawError "Originating client no-longer exists")
                    
  return ("reply", rsp)


-- | Send a message back to the client that started the call/response
--   query. This is complicated by the way I am handling callAndWait
--   calls, where the response should be placed in the MVar rather
--   then sent directly.
--
--   TODO: a lot of repeated code with sendToHub
replyToOrigin ::
    ClientName       -- ^ name of the client making the response
    -> InFlightData
    -> SAMPResponse
    -> IO ()
replyToOrigin clName ifd replyData = do
  let url = ifdUrl ifd
      secret = ifdSecret ifd
      tag = ifdTag ifd
      rsp = SAMPReturn (toSValue replyData)
            
  case idfMRsp ifd of
    Just var -> atomically (putTMVar var rsp)
    _ -> sendReceiveResponse url secret clName tag replyData


-- | Notify a client about a message.
--
--   TODO: should this note down problematic clients so that they
--         can get removed (or something else done to them?)
--
--   TODO: should just be sent the ClientData structure, rather than
--         name/secret/url (and if not callable it's a nop)
--
notifyRecipient ::
    RString
    -- ^ the name of the client sending the notification
    -> SAMPMessage
    -- ^ the message to notify recipient about
    -> ClientData
    -- ^ the recipient; if it is not callable then this does nothing.
    -> IO ()
    -- ^ any error from the call is logged (debug level)
    --   but otherwise ignored
notifyRecipient sendName msg receiver = do
    let recName = cdName receiver
        recSecret = cdSecret receiver
        mtype = getSAMPMessageType msg
                    
        arglist = [toSValue recSecret, SAMPString sendName, toSValue msg]
        nargs = map XI.toValue arglist
        act url = void (HC.call url "samp.client.receiveNotification" nargs)
        hdl url e = dbgIO ("Error in sending " ++ show mtype ++ " to " ++
                           url ++ " - " ++ e)

    case cdCallback receiver of
      Just url -> do
        let fname = getFullName receiver
            name = show recName ++ if null fname then "" else " " ++ fname
        {-
        putStrLn ("*** " ++ show mtype ++ " from " ++ show sendName ++
                  " to " ++ show recName)  -- DBG
        -}
        dbgIO ("Sending notify of " ++ fromMType mtype ++ " to client " ++
               name)
        handleError (hdl url) (act url)
                        `CE.catch` ignoreNotifyError recName fname mtype

      Nothing -> return ()


-- | Notify a client about a message using the call protocal.
--
--   TODO: should this note down problematic clients so that they
--         can get removed (or something else done to them?)
--
--   This does NOT update the hub to record the in-flight message.
--
--   This does nothing if the client is not callable, but this should
--   not be the case (since the message id should not have been
--   created).
--
clientReceiveCall ::
    ClientName  -- ^ the name of the client sending the message
    -> ClientData -- ^ the client to send the message to
    -> MessageId -- ^ Send this to the client
    -> SAMPMessage -- ^ the message to send
    -> IO ()
       -- ^ any error from the call is logged (debug level)
       --   but otherwise ignored
clientReceiveCall sendName receiver msgId msg = 
    let rSecret = cdSecret receiver
        rName = cdName receiver
        mtype = getSAMPMessageType msg
                
        msgMap = toSValue msg
        arglist = [ toSValue rSecret
                  , toSValue sendName
                  , toSValue msgId
                  , msgMap ]
        nargs = map XI.toValue arglist
        hdl url e = dbgIO ("Error in sending " ++ show mtype ++ " to " ++
                           url ++ " - " ++ e)

        act url = void (HC.call url "samp.client.receiveCall" nargs)

    in case cdCallback receiver of
         Just url -> do
           let fname = getFullName receiver
               name = show rName ++ if null fname then "" else " " ++ fname
           dbgIO ("Sending call of " ++ fromMType mtype ++ " to client " ++
                  name)
           handleError (hdl url) (act url)
                           `CE.catch` ignoreNotifyError rName fname mtype

         _ -> return ()


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

-- is there a more-direct way to go from Int64 to LT.Text? Too laxy to look.
toLen :: L.ByteString -> LT.Text
toLen = LT.pack . show . L.length
            
-- base-64encoded version of the logo (as a PNG)
logo64_len :: LT.Text
logo64_len = toLen logo64

-- The logo is the lambda character surrounded by a circle.
-- I created this using inkscape, and converted to base64
-- encoding to store here.
logo64 :: L.ByteString
logo64 = decodeLenient "iVBORw0KGgoAAAANSUhEUgAAAEcAAABICAYAAACk5ujKAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAFQwAABUMB7A6f7AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAeWSURBVHic1Zx50NVTGMc/XTFaFFq0IVuFyV72LcQYBikTWgwxmAmjGktl34oxzFjLrsm+FVlHyR8IwyB6o1eUJYR6U+/bm/f647m/1/W7zznnt5xz33xnnrkz93fO8/2ec8/vLM8557aiutgKOBDYDegD9AW6Ae1L1gFYDawB/gJ+AmpK9iXwHvBnlTUHxUHALcACYANQzGEbgA+Am5FK/l+iFzAR+cXzVIbLFgFXAj2rU6x82BGYBjQQtlLi1gDcB/QOXsIM6Aw8ADRS3UqJ23rgfqBT2OImQytgFPALLVspcfsduBgohCu6HdsAb1kEpi3MMmBJ6fN3T37fALqGqgATDgd+zCC2HpgLTAZOQobzTQ0cmyJD/cnAVcC8Uv60nMuBQ/0U243zSde3rAdeAoYAbXJytwVOBWan1NAInJeT24krUwiqA6Yik7wQ6A7cWuJJoqcJuCKQFqYmFNEI3E71RozOwJ0kn2BO8S3gsoTEC4C9fJMnxD7Ahwk0FoEJvkjPRJqkq8neCWzmizQjWiNLlSR6R+clOwDpUG1Ea5HO1oXBSKd4LjAMOB7YGZkr+cZQYJ1Bb2QNwICsBFsCtQ6COuCohP6eN/hYDtyErNh94jBkBW/TvwTomMX50w7Hq4B9U/gzVU5k3wP9swi1YF9Ep433ybROhzgcNgDHpPTpqpwi8AOwdVqxDhyBe/J4UlJnbYGlDmcjMoi8BnizZDWYh947Mvh2YbSBK7JaEk5Qb3A4esCT4P7Ap4r/1UA7TxzleEjhKrdrXA46IyFKk4OvkJblC92BnxWekR45IrTDHnyrw/FKX2/JXAQGBRA9TuF5KwAPyMhqK9/VpoztgN8sGZ8IJLgDEkwv5/ob2DYQ3zOYy7gS2ELLdJ4lUyNhQ5AzFM5Qi8SdsK/DxmiZ3rVkeCyQ0AiDFc4awsyeAWYqfJG9HU+8A/b1iO/JWRwFZBIY590/EN8eClf5K71dJApkLWL6lRYAnwcSGaEJ+TXjGB2I7zPgI8OzAhJQa64c2yj0uEdRNnyjfDcc2DwQn/ZjRGiuj9bIxEtrYk2Ei+TF8YlBw9BAfD0NfEVkwboJyHttSvRFIGFxHGvRMDsg71cW3gEFYE9L5nkBhZVjnOXZcYRrvfMsz/oXgF0sCT70q0UXARxted4aOD0Q9wLLs74FZG/IhEWexWiYgHs+c3Yg7hrLsz4gw5rpvfMdW4mjJ/qhAy3EGSJw30nhiezTArK20VCPbMuGxEVUBuVfRx9mRwXgX4nEyDV0iBJoNfdrADHl2AL4Q+EdhGzfxr9fgXnrOA+s5Tedpfk2gJByXKpwflJ61gr4Wnl+QgAdSxWeIlDfUkc0WiOvVBw3lz5Ni91QywkjWuK1OkPhq0UqLUIvKkMLDfjfZraW/zvDw3rPIsrxgcJ3oZJOO/9zgWctpm6lFmTFXc2hfJDCswI9+j9SSfueRy2dFf/NQznALEuCAzwKifCKwjPZkLYN+q5lP09aDlZ8R/ZCAfss0TZ7zoJ+yFqpHGuBew3p1wHPKt/72p3oY3m2uAAstiQY6ElEhAlUHmKcjgT2TXhE+W40pZBCTtgijTUgFWBqWgs9CIjQlcplQSOwfYK8ixRtSQ8w2GDbx9oP3MGu7h5EgL6TOiNh3klK3rxB/16Kz8iag10AcywJx+YUAbJLqu2J7Z0wvzbnWYNhjykhtBl6ZLPKE463JLTFPJJirOL31ZQ+3lR8nJVD00eKv8guKU/YG/vWzB45RGyCBM/jPtNuLY9QfMzNqGlPxVdkfyMt9T+Yb8mQtG/QMEzxlyXC2IbKVXwTchklLZ5QNEVWsakH7u3gLCJAZrRxf8My+pqu+DJNIE3YGft28DlaJtdBgqdSigA9LrOE7HMUbUb7Nem2jZ9TfES2ErkxqOJaS8YilbNbF15UfORdOGrbKQcnzHu0krfcrrJl7oT98FINyU9d9UE6t/L8pgVmGkxUdE1LkK89showlW01CU60XmdxUAQeTliI+5S8kxLmtaEnlX3GKtwnzh5V9FhbjfautkWWDb0tRGeVyGy4iMpWMh0/QfuzgS6x72Yid7VM6R+0+KsFdidhDOtk7LW8HjlT83/AkbiP2p6Y1umTDoerKC3ONmIMwLxujMx22sKI9rivPteR/rB2tXA47tPr32Det3NiIO5r0OuA07ISBMJw3K9SPR5a/nAqh2TN7mfjuVLk0tqEx7POExIQFoGPabl+aCDmA1BxG++bfEpC4g3AXVQOtaHQFbiHZK27iLSsILgc9y24yNYAt+EvkhhHD+QuafyAt+1VuiyQlmaMIf215dnISjzvhY92SOf/ckoN6zGstkPgEOSGXVJxkTUA7yB3DE4BdsXciW+G/M/OEORGy3yy/YHIMuRvaqqKLsBrGcRq9idS2UtKn675SVKbg+xqthiGIattH4XxZSuR4F2o6wGpsDWya+m6SRzaGpCR0vdlWi/YHqmkLH/Ikcfqgbsp3VnY2NEDGfZth6B92EJkeA41XQiO/YEbgffJ/4dmjUjA/gZyXJhPimp3WB3596/w+iKh1G7IzmX70vNVyASyDrn/WYOENxciFbO6WmL/AWeEE8KboxuqAAAAAElFTkSuQmCC"

                                             
createHub :: Socket -> HubInfo -> HubSecret -> IO ()
createHub socket hi secret = do
    mware <- logRequests
    scottySocket def socket $ do
        middleware mware

        -- Note that the XML-RPC standard mandates that both
        -- Content-Type and Content-Length headers are present
        -- in the response. http://xmlrpc.scripting.com/spec.html
        post "/xmlrpc/" $ do
          dbg "Processing a post call..."
          reqType <- header cType
          dbg ("  Content-Type: " ++ show reqType)
          if reqType == Just "text/xml"
            then callFromClient hi secret
            else invalidCT

        get "/logo.png" $ do
          dbg "Request for the logo"
          setHeader cType "image/png"
          setHeader cLength logo64_len
          raw logo64

        get "/api/clients.json" $ do
          dbg "Request for clients: json format"
          out <- liftIO (jsonGetClients hi)
          json out

        get "/api/metadata.json" $ do
          dbg "Request for client metadata: json format"
          out <- liftIO (jsonGetMetadata hi)
          json out
               
        get "/api/subscriptions.json" $ do
          dbg "Request for subscriptions: json format"
          out <- liftIO (jsonGetSubscriptions hi)
          json out

        -- very insecure!
        get "/api/callbacks.json" $ do
          dbg "Request for callbacks: json format"
          out <- liftIO (jsonGetCallbacks hi)
          json out
              
        return ()

{-
XML-RPC is done via a HTML POST of an XML document.
At present I enforce the content-type restriction but
this may be a bit OTT.
-}

invalidCT :: ActionM ()
invalidCT = do
  status status400
  let msg = "400 Bad Request (missing or invalid Content-Type header)"
  setHeader cLength ((LT.pack . show . LT.length) msg)
  text msg

-- It appears that ds9 7.3.2 does not handle HTTP header keywords
-- as case insensitive.
cType, cLength :: LT.Text
cType = "Content-Type"
cLength = "Content-Length"
          
{- | Process the calls from the SAMP clients.

TODO: re-work this code
-}

callFromClient :: HubInfo -> HubSecret -> ActionM ()
callFromClient hi secret = do
  b <- body
  let str = L.unpack b
      act = parseSAMPCall str
  mmethod <- liftIO (handleError (return . Left) (Right <$> act))
  case mmethod of
    Right method -> handleSAMP hi secret method
    -- not sure 400 is the best response here
    Left emsg -> do
      dbg ("*** Invalid SAMP call: " ++ emsg)
      status status400
      text "Invalid SAMP call"

        
handleSAMP ::
    HubInfo
    -> HubSecret
    -> SAMPMethodCall
    -> ActionM ()
handleSAMP hi secret (SAMPMethodCall name args) = do
  liftIO (dumpHub hi)
  let mname = fromMType name
  dbg ("Server called with method: " ++ mname)
  dbg ("  args: " ++ show args)

  -- To avoid an Ord constraint on MType, use a HashMap rather than
  -- Map.
  case HM.lookup name (hiHubHandlers (hiReader hi)) of
    Just f -> do
      (lbl, rsp) <- liftIO (f hi secret args)
      respondXmlRpc lbl rsp
                    
    _ -> let emsg = "Unsupported message: " ++ mname
         in respondXmlRpc "Unknown message" (rawError emsg)


-- The XML-RPC standard mandates that both the Content-Type and
-- Content-Length headers are included in the response. This is
-- probably handled by haxr's server code, but I am not using that here.

respondXmlRpc :: String -> SAMPMethodResponse -> ActionM ()
respondXmlRpc lbl srsp = do
  let rsp = HC.fromSAMPMethodResponse srsp
  dbg ("Response [" ++ lbl ++ "] " ++ L.unpack rsp)
  setHeader cType "text/xml"
  setHeader cLength (toLen rsp)
  raw rsp


respondToClient :: String -> IO SAMPMethodResponse -> IO (String, SAMPMethodResponse)
respondToClient = respond
                  
-- A helper function whilst converting code; this is meant to be
-- a stop-gap function, to be removed at a later date
respond :: String -> IO SAMPMethodResponse -> IO (String, SAMPMethodResponse)
respond lbl act = (\a -> (lbl, a)) <$> act

                  
-- An empty response
respondOkay :: HubFunc
respondOkay _ _ _ = return ("okay", emptyResponse)


-- Errors can be returned as a SAMP Response - e.g.
--     toSAMPResponseError msg []
-- but it looks like most (hopefully all) cases that respondError
-- is used should return an XML-RPC fault.
--
-- TODO: clean up the naming
respondError :: String -> IO (String, SAMPMethodResponse)
respondError = respond "error" . return . rawError

rawError :: String -> SAMPMethodResponse
rawError = SAMPFault

reportError :: String -> IO SAMPMethodResponse
reportError emsg = do
    dbg emsg
    return (rawError emsg)

           
handleRegister :: HubFunc
handleRegister _ _ [] = respondError "Missing hub secret"
handleRegister hi secret (SAMPString s:_)
    | fromHubSecret secret == s = register hi
    | otherwise = respondError "Invalid hub secret"
handleRegister _ _ _ = respondError "Invalid hub secret"


handleUnregister :: HubFunc
handleUnregister hi _ (SAMPString s:_) = unregister hi (toClientSecret s)
handleUnregister _ _ _ = respondError "Invalid arguments"


handleDeclareMetadata :: HubFunc
handleDeclareMetadata hi _ (SAMPString s : SAMPMap lvs : _) =
    declareMetadata hi (toClientSecret s) lvs
handleDeclareMetadata _ _ _ = respondError "Invalid arguments"


handleSetXmlrpcCallback :: HubFunc
handleSetXmlrpcCallback hi _ (SAMPString s : SAMPString url : _) =
    setXmlrpcCallback hi (toClientSecret s) url
handleSetXmlrpcCallback _ _ _ = respondError "Invalid arguments"


handleDeclareSubscriptions :: HubFunc
handleDeclareSubscriptions hi _ (SAMPString s : SAMPMap kvs : _) =
    case sampToSubMap kvs of
      Right subs -> declareSubscriptions hi (toClientSecret s) subs
      Left emsg -> respondError emsg
handleDeclareSubscriptions _ _ _ = respondError "Invalid arguments"


-- do we need MType <-> SAMPString conversion routines?
sampToSubMap :: SAMPMapValue -> Either String SubscriptionMap
sampToSubMap ms = 
    let conv :: SAMPMapElement -> Either String (MType, SAMPMapValue)
        conv (mmt, SAMPMap xs) = do
          mt <- handleError Left (toMTypeE (fromRString mmt))
          Right (mt, xs)
        conv (mmt, _) = Left (fromRString mmt ++ " was not sent map")

        kvs = M.toList ms
        (bad, good) = partitionEithers (map conv kvs)

        badMsg = "Invalid subscription map: " ++
                 intercalate ", " bad

    in if null bad then Right (toSubMap good) else Left badMsg

                                                   
handleGetRegisteredClients :: HubFunc
handleGetRegisteredClients hi _ (SAMPString s : _) =
    respondToClient "get/reg clents" (getRegisteredClients hi (toClientSecret s))
handleGetRegisteredClients _ _ _ = respondError "Invalid arguments"


handleGetSubscribedClients :: HubFunc
handleGetSubscribedClients hi _ (SAMPString s : SAMPString msg : _) =
    case toMType (fromRString msg) of
      Just mtype -> if isMTWildCard mtype
                    then respondError "MType can not include a wild card"
                    else
                        respondToClient "get/sub clients"
                                        (getSubscribedClients hi (toClientSecret s) mtype)
      _ -> respondError "Invalid MType"
handleGetSubscribedClients _ _ _ = respondError "Invalid arguments"


handleGetMetadata :: HubFunc
handleGetMetadata hi _ (SAMPString s : SAMPString name : _) =
    respondToClient "get/metadata" (getMetadata hi (toClientSecret s) (toClientName name))
handleGetMetadata _ _ _ = respondError "Invalid arguments"


handleGetSubscriptions :: HubFunc
handleGetSubscriptions hi _ (SAMPString s : SAMPString name : _) =
    respondToClient "get/subs" (getSubscriptions hi (toClientSecret s) (toClientName name))
handleGetSubscriptions _ _ _ = respondError "Invalid arguments"

                             
handleNotify :: HubFunc
handleNotify hi _ (SAMPString s : SAMPString rid : msgArg : _) =
    case handleError Left (fromSValue msgArg) of
      Right msg -> notify hi (toClientSecret s) (toClientName rid) msg
      Left emsg -> respondError emsg
handleNotify _ _ _ = respondError "Invalid arguments"


handleNotifyAll :: HubFunc
handleNotifyAll hi _ (SAMPString s : msgArg : _) =
    case handleError Left (fromSValue msgArg) of
      Right msg -> notifyAll hi (toClientSecret s) msg
      Left emsg -> respondError emsg
handleNotifyAll _ _ _ = respondError "Invalid arguments"

                      
handleCall :: HubFunc
handleCall hi _ (SAMPString s : SAMPString rid :
                 SAMPString tagstr : msgArg : _) =

    let tag = toMessageTag tagstr
        secret = toClientSecret s
        clName = toClientName rid
    in case handleError Left (fromSValue msgArg) of
         Right msg -> sendToClient hi secret clName tag msg
         Left emsg -> respondError emsg

handleCall _ _ _ = respondError "Invalid arguments"


handleCallAll :: HubFunc
handleCallAll hi _ (SAMPString s : SAMPString tagstr : msgArg : _) =
    let tag = toMessageTag tagstr
        secret = toClientSecret s
    in case handleError Left (fromSValue msgArg) of
         Right msg -> sendToAll hi secret tag msg
         Left emsg -> respondError emsg

handleCallAll _ _ _ = respondError "Invalid arguments"


handleCallAndWait :: HubFunc
handleCallAndWait hi _ (SAMPString s : SAMPString rid : msgArg :
                        timeoutArg : _) =
    let secret = toClientSecret s
        clName = toClientName rid

        argsE = do
          msgVal <- fromSValue msgArg
          timeoutVal <- fromSValue timeoutArg
          return (msgVal, timeoutVal)
                 
    in case handleError Left argsE of
         Right (msg, tout) -> callAndWait hi secret clName tout msg
         Left emsg -> respondError emsg

handleCallAndWait _ _ _ = respondError "Invalid arguments"

                        
handleReply :: HubFunc
handleReply hi _ (SAMPString s : SAMPString msg : rspArg : _) =
    let msgId = toMessageId msg
        secret = toClientSecret s

    in case handleError Left (fromSValue rspArg) of
         Right rsp -> reply hi secret msgId rsp
         Left emsg -> respondError emsg

handleReply _ _ _ = respondError "Invalid arguments"

                    
-- To share the same code between async and synchronous calls,
-- we need a tag for the synchronous cases. This may indicate
-- that the code should be re-written (since the handling in the
-- synchronous case can be a lot simpler than the synchronous case).
--
dummyTag :: MessageTag
dummyTag = toMessageTag (error "dummy tag value should not be used")


-- | Map from the client identifier (the secret value) to metadata
type ClientMap = M.Map ClientSecret ClientData

-- | Metadata for a client or hub
type MetadataMap = M.Map RString SAMPValue


-- | Map of subscriptions for a client, and any associated
--   metadata.
--
--   This is a HashMap rather than Map so that the key (MType)
--   does not have to be ordered. It is a newtype so that
--   the caller has to use (or, rather, should use) the
--   access routines provided, rather than use the HashMap
--   API directly.
--
newtype SubscriptionMap = SM (HM.HashMap MType SAMPMapValue)
    deriving (Eq, Show)

emptySubMap :: SubscriptionMap
emptySubMap = SM HM.empty

-- | This structure should be a union type, since if ifdMRsp is not
--   None then some of the other fields are not needed. Not
--   worth re-factoring at this time as this is all in flux.
data InFlightData =
    IFD { ifdSecret :: ClientSecret
        , ifdUrl :: String
        , ifdTag :: MessageTag
        , idfMRsp :: Maybe (TMVar SAMPMethodResponse)
        , ifdTime :: UTCTime
        -- ^ the approximate time the response was sent
        }
    -- deriving (Eq, Show)
             
-- | Represent "in-flight" messages; that is, information requests
--   from one client to another mediated by the hub. The originating
--   client sends in a MessageTag to indicate the message. Communication
--   between the hub and the recipient uses a MessageId, and so the
--   hub needs to know a mapping from MessageId to the originating
--   client.
--
type InFlightMap = M.Map MessageId InFlightData

-- | Does the MType exist in the map. The MType *must not* include
--   a wildcard (but the subscription map can).
--
--   It currently does *not* handle x-samp/samp equivalence.
memberSubMap :: MType -> SubscriptionMap -> Bool
memberSubMap k m = not (null (lookupSubMap k m))

-- | This returns *all* subscriptions (which can include wild
--   cards) that matches the message (which MUST NOT contain a
--   wild card).
--
lookupSubMap ::
    MType -- ^ it is an error for this to contain a wildcard
    -> SubscriptionMap
    -> [(MType, SAMPMapValue)] -- ^ empty if there is no match
lookupSubMap k (SM m) =
    if isMTWildCard k
    then -- ideally this would be an impossible condition
        error ("Internal error: sent a wildcard MType: " ++ fromMType k)
    else filter (\t -> fst t == k) (HM.toList m)
                        
toSubMap :: [(MType, SAMPMapValue)] -> SubscriptionMap
toSubMap = SM . HM.fromList

fromSubMap :: SubscriptionMap -> [(MType, SAMPMapValue)]
fromSubMap (SM m) = HM.toList m

{-
TODO: perhaps have a specialized version of the subscription map so
that users are forced to use its own versions of the map API,
so that this can then be changed internally to handle the support
of x-samp.* messages.
-}
    
data ClientData =
    ClientData {
      cdName :: ClientName     -- ^ name of the client
    , cdSecret :: ClientSecret -- ^ the identifier/secret value for this client
    , cdSubscribed :: SubscriptionMap  -- ^ messages the client is subscribed to
    , cdMetadata :: MetadataMap
    , cdCallback :: Maybe String
    -- ^ URL for XML callback; use string rather that the URI type since
    --   haxr requires strings 
    , cdInFlight :: InFlightMap
    -- ^ This should only be used if the callback is set
    , cdNextInFlight :: Int
    } -- deriving (Eq, Show)


-- Extract the samp.name metadata element if it exists
getFullName :: ClientData -> String
getFullName client = case M.lookup "samp.name" (cdMetadata client) of
  Just (SAMPString fn) -> "(" ++ show fn ++ ")"
  _ -> ""
                      

{-
Might it also be useful to keep a map of clients registered
to a particular MType?
-}

-- | These functions are called by the hub to process a message.
--
--   The MType must not contain a wild card.
--
--   At present the functions could be pure, but leave in IO
--   for future expansion.
--
type HubHandlerFunc =
    HubInfo
    -> SAMPMessage
    -> IO SAMPResponse

type HubFunc =
    HubInfo
    -> HubSecret
    -> [SAMPValue]
    -> IO (String, SAMPMethodResponse)

       
makeHubSubscriptions :: [(MType, HubHandlerFunc)] -> SubscriptionMap
makeHubSubscriptions hdlrs =
    let emap = M.empty
        kvs = map (\(a,_) -> (a,emap)) hdlrs
    in toSubMap kvs


defaultHubHandlers :: HM.HashMap MType HubFunc
defaultHubHandlers =
    HM.fromList [
          ("samp.hub.ping", respondOkay)
         , ("samp.hub.register", handleRegister)
         , ("samp.hub.unregister", handleUnregister)
         , ("samp.hub.setXmlrpcCallback", handleSetXmlrpcCallback)
         , ("samp.hub.declareMetadata", handleDeclareMetadata)
         , ("samp.hub.getMetadata", handleGetMetadata)
         , ("samp.hub.declareSubscriptions", handleDeclareSubscriptions)
         , ("samp.hub.getSubscriptions", handleGetSubscriptions)
         , ("samp.hub.getRegisteredClients", handleGetRegisteredClients)
         , ("samp.hub.getSubscribedClients", handleGetSubscribedClients)
         , ("samp.hub.notify", handleNotify)
         , ("samp.hub.notifyAll", handleNotifyAll)
         , ("samp.hub.call", handleCall)
         , ("samp.hub.callAll", handleCallAll)
         , ("samp.hub.callAndWait", handleCallAndWait)
         , ("samp.hub.reply", handleReply)
         ]

-- | This information is not going to change whilst processing a
--   request.
--
--   Should this just embed a ClientData for the hub itself?
data HubInfoReader =
    HubInfoReader {
      hiName :: RString -- the name of the hub
    , hiSecret :: HubSecret -- the secret for the hub
    , hiSecretLen :: Int    -- number of characters in the client secrets
    , hiMetadata :: MetadataMap
    , hiSubscribed :: SubscriptionMap
    -- note: two sets of handlers
    , hiHandlers :: [(MType, HubHandlerFunc)]
    , hiHubHandlers :: HM.HashMap MType HubFunc
    , hiCallback :: String  -- URL for the hub; need to think about this
    } -- deriving Show

-- | This information may change due to processing a request
data HubInfoState =
    HubInfoState {
      hiRandomGen :: StdGen -- generator used for creating random sequences
      , hiClients :: ClientMap
      , hiNextClient :: Int -- the id for the next client
    } -- deriving Show

-- The hub could be considered as a client, and added as the
-- first entry in HubInfoState - perhaps using some form of a
-- non-empty list - but then this does not promise that the
-- information is immutable (perhaps there's a reason to make
-- the metadata/subscription info mutable for the hub?).
-- It would make things easier when processing requests,
-- but for now separate out.
--
data HubInfo =
    HubInfo {
      hiReader :: HubInfoReader
    , hiState :: TVar HubInfoState
    }
                  
dumpHub :: HubInfo -> IO ()
dumpHub hi = do
  let hir = hiReader hi
      mvar = hiState hi
  his <- atomically (readTVar mvar)
  dbgIO ("Hub: " ++ fromRString (hiName hir))
  dbgIO ("  next client: " ++ show (hiNextClient his))
  dbgIO "  clients:"
  let clmap = hiClients his
      n = M.size clmap
      dump (i, clinfo) = do
          let str = " " ++ show i ++ " / " ++ show n ++ " " ++
                    show (cdName clinfo)
          dbgIO str
          dbgIO ("    metadata=" ++ show (cdMetadata clinfo))
                   
  forM_ (zip [1::Int ..] (M.elems clmap)) dump

    
appName, appDesc, appAuthor, appAffil, appEmail :: SAMPValue
appName = "hsamp-hub"
appDesc = "An example SAMP hub, written in Haskell."
appAuthor = "Douglas Burke"
appAffil = "Chandra X-ray Center, Smithsonian Astrophysical Observatory"
appEmail = "dburke@cfa.harvard.edu"

           
newHubInfo ::
    String          -- ^ base URL of the hub
    -> StdGen       -- ^ The generator to use
    -> HubSecret    -- ^ The secret to use
    -> IO HubInfo
newHubInfo huburl gen secret = do
  let miconurl = toRString (huburl ++ "logo.png")
      hmdata1 = M.fromList [ ("samp.name", appName)
                           , ("samp.description.text", appDesc)
                           , ("author.name", appAuthor)
                           , ("author.mail", appEmail)
                           , ("author.affiliation", appAffil)
                           ]
      hmdata = case miconurl of
                 Just u -> M.insert "samp.icon.url" (SAMPString u) hmdata1
                 _ -> hmdata1

      handlers = [ ("samp.app.ping", hubSentPing)
                 , ("samp.query.by-meta", hubSentQueryByMeta)
                 , ("x-samp.query.by-meta", hubSentQueryByMeta)
                 ]
      hsubs = makeHubSubscriptions handlers
      hir = HubInfoReader {
              hiName = "hub"
            , hiSecret = secret
            , hiSecretLen = 16
            , hiMetadata = hmdata
            , hiSubscribed = hsubs
            , hiHandlers = handlers
            , hiHubHandlers = defaultHubHandlers
            , hiCallback = addXMLRPCEndPoint huburl
            }
      his = HubInfoState {
              hiRandomGen = gen
            , hiClients = M.empty
            , hiNextClient = 1
            }
  tvar <- newTVarIO his
  return HubInfo { hiReader = hir, hiState = tvar }


addClient ::
    HubInfoReader
    -> HubInfoState
    -> (ClientName, ClientSecret, HubInfoState)
       -- ^ Name of the client, the client secret, and the updated hub info
addClient hr ohi =
    -- Note that the client secret has a prefix, related to the client
    -- number, just to make sure that it is unique.
  let (secret, ngen) = randomAlphaNumRString (hiSecretLen hr) (hiRandomGen ohi)
      fromNum = fromJust . toRString . show
      secretVal = toClientSecret (mconcat ["c", fromNum clNum, ":", secret])
      clNum = hiNextClient ohi
      nameStr = "cl" ++ show clNum
      emsg = "Unable to convert " ++ nameStr ++ " to a RString!"
      clName = toClientName (fromMaybe (error emsg) (toRString nameStr))
      oclientmap = hiClients ohi
      client = ClientData { cdName = clName
                          , cdSecret = secretVal
                          , cdSubscribed = emptySubMap
                          , cdMetadata = M.empty
                          , cdCallback = Nothing
                          , cdInFlight = M.empty
                          , cdNextInFlight = 1
                          }
      nclientmap = M.insert secretVal client oclientmap
  in (clName, secretVal,
      ohi { hiRandomGen = ngen
          , hiClients = nclientmap
          , hiNextClient = succ clNum })


addClientToHub ::
    HubInfo
    -> IO (ClientName, ClientSecret)
addClientToHub hi = do
  (ans, sact) <- changeHub hi $ \ohub -> 
      let (client, secret, nhub) = addClient (hiReader hi) ohub
          -- try to enforce strict semantics for the changes in addClient,
          -- which is ugly and prone to my mis-understandings of things
          sact = hiRandomGen nhub `seq` 
                 hiClients nhub `seq`
                 hiNextClient nhub `seq`
                 secret `seq` return ()

      in (nhub, ((client, secret), sact))

  sact
  return ans


-- TODO: in the broadcast code; should we identify problematic
-- clients (i.e. those that don't respond) so that they can be
-- removed?

-- TODO: do I need to include "other arguments" here?

broadcastMType ::
    HubInfo
    -> ClientName  -- ^ the client that sent the message
    -> SAMPMessage -- ^ the message
    -> IO ()
broadcastMType hi name msg =
  withHub hi $ \hub -> do
    let sendName = fromClientName name
        mtype = getSAMPMessageType msg
        clients = findOtherSubscribedClients (hiClients hub) name mtype
        hir = hiReader hi

    forM_ clients (forkCall . notifyRecipient sendName msg)
        
    -- TODO: do I need to fix hub handling of the message or is this okay?
    --       
    when (hiName hir /= fromClientName name &&
          memberSubMap mtype (hiSubscribed hir))
      (dbgIO ("**** skipping notifying hub about " ++ show mtype))


-- The client name is included so that it can be excluded from the
-- broadcast list (in case the metadata registration for this
-- client has been processed before this routine is called, which
-- appears to be unlikely).
--
broadcastNewClient ::
  HubInfo
  -> ClientName  -- ^ the client that has just registered
  -> IO ()
broadcastNewClient hi name =
    let hubName = toClientName (hiName (hiReader hi))
        msg = toSAMPMessageMT  "samp.hub.event.register" params M.empty
        params = M.singleton "id" (toSValue name)
    in broadcastMType hi hubName msg
           

broadcastRemovedClient ::
  HubInfo
  -> ClientName  -- ^ the client that has just been removed
  -> IO ()
broadcastRemovedClient hi name =
    let hubName = toClientName (hiName (hiReader hi))
        msg = toSAMPMessageMT "samp.hub.event.unregister" params M.empty
        params = M.singleton "id" (toSValue name)
    in broadcastMType hi hubName msg


forkCall :: IO a -> IO ()
forkCall act = void (forkIO (void act))


makeResponse :: SAMPMapValue -> SAMPMethodResponse
makeResponse = SAMPReturn . SAMPMap

               
{-
The response to a message - i.e. sending the notifications - can
be done in a separate thread. Depending on what gets sent to the
action (i.e. whether it is sent the hub state at the time of
the processing, or an MVar), there may be the chance for confusion,
but worry about that later.
-}

register ::
    HubInfo
    -> IO (String, SAMPMethodResponse)
register hi = do
  let hubName = hiName (hiReader hi)
  (clName, clKey) <- addClientToHub hi
  let kvs = M.fromList [ ("samp.private-key", toSValue clKey)
                       , ("samp.self-id", toSValue clName)
                       , ("samp.hub-id", toSValue hubName) ]

  dbg ("Added client: " ++ show clName ++ " secret=" ++ show clKey)
  -- forkCall (broadcastNewClient hi clName)
  -- As broadcastMType includes forkCall statements, do not fork
  -- here (which may slow down the response a bit)
  broadcastNewClient hi clName
  return ("register", makeResponse kvs)


unregister ::
    HubInfo
    -> ClientSecret
    -> IO (String, SAMPMethodResponse)
unregister hi secret = do
  rsp <- changeHubWithClient hi secret $ \ohub sender ->
    let oclientmap = hiClients ohub
        nclientmap = M.delete secret oclientmap
        nhub = ohub { hiClients = nclientmap }
        clName = cdName sender
                 
        sact = do
          dbg ("Removing client " ++ show clName)
          nclientmap `seq` nhub `seq` return ()
          unless (M.null (cdInFlight sender))
            (warn ("Non empty inFlightMap: " ++
                   show (M.keys (cdInFlight sender))))
          broadcastRemovedClient hi clName
          -- forkCall (broadcastRemovedClient hi clName)

      in (nhub, emptyResponse, sact)

  return ("unregister", rsp)


declareMetadata ::
    HubInfo
    -> ClientSecret
    -> SAMPMapValue
    -> IO (String, SAMPMethodResponse)
declareMetadata hi secret mdata = do
  rsp <- changeHubWithClient hi secret $ \ohub sender ->
    let nsender = sender { cdMetadata = mdata }
        oclientmap = hiClients ohub
        nclientmap = M.insert secret nsender oclientmap
        nhub = ohub { hiClients = nclientmap }
                 
        hubName = toClientName (hiName (hiReader hi))
        params = M.fromList [ ("id", toSValue (cdName sender))
                            , ("metadata", SAMPMap mdata)]
        msg = toSAMPMessageMT "samp.hub.event.metadata" params M.empty
         
        -- QUS: does this fully evaluate everything?
        sact = do
          dbg ("Registering new metadata for client " ++
               show (cdName sender))
          mdata `seq` nsender `seq` nclientmap `seq` nhub `seq` return ()
          broadcastMType hi hubName msg
          -- forkCall (broadcastMType hi hubName msg)

      in (nhub, emptyResponse, sact)

  return ("set/metadata", rsp)


setXmlrpcCallback ::
    HubInfo
    -> ClientSecret
    -> RString        -- ^ the URL for the callback   
    -> IO (String, SAMPMethodResponse)
setXmlrpcCallback hi secret urlR = do
  let url = fromRString urlR

      act ohub sender =
        let nsender = sender { cdCallback = Just url }
            oclientmap = hiClients ohub
            nclientmap = M.insert secret nsender oclientmap
            nhub = ohub { hiClients = nclientmap }
                 
            sact = do
              dbg ("Registering Callback for client " ++
                   show (cdName sender) ++ " : " ++ url)
              nsender `seq` nclientmap `seq` nhub `seq` return ()
           
        in (nhub, emptyResponse, sact)
        
  rsp <- case parseURI url of
           Just _ -> changeHubWithClient hi secret act
           -- TODO: is this the correct error?
           Nothing -> return (rawError "Invalid url")

  return ("set/callback", rsp)


declareSubscriptions ::
    HubInfo
    -> ClientSecret
    -> SubscriptionMap
    -> IO (String, SAMPMethodResponse)
declareSubscriptions hi secret subs = do
  rsp <- changeHubWithCallableClient hi secret $ \ohub sender ->
    let nsender = sender { cdSubscribed = subs }
        oclientmap = hiClients ohub
        -- replace sender by nsender in the map
        nclientmap = M.insert secret nsender oclientmap
        nhub = ohub { hiClients = nclientmap }

        hubName = toClientName (hiName (hiReader hi))
        conv (mt, xs) = (fromMTypeRS mt, SAMPMap xs)
        kvs = M.fromList (map conv (fromSubMap subs))
        params = M.fromList [ ("id", toSValue (cdName sender))
                            , ("subscriptions", SAMPMap kvs) ]
        msg = toSAMPMessageMT "samp.hub.event.subscriptions" params M.empty

        -- QUS: does this fully evaluate everything?
        sact = do
          dbg ("Registering new subscriptions for client " ++
               show (cdName sender))
          subs `seq` nsender `seq` nclientmap `seq` nhub `seq` return ()
          broadcastMType hi hubName msg
          -- forkCall (broadcastMType hi hubName msg)
                          
    in (nhub, emptyResponse, sact)

  return ("set/subs", rsp)
    

getRegisteredClients ::
    HubInfo
    -> ClientSecret
    -> IO SAMPMethodResponse
getRegisteredClients hi secret =
    withClient hi secret $ \hub sender -> do
      let clMap = hiClients hub
          hubNames = hiName (hiReader hi)
          clNames = mapMaybe selClient (M.elems clMap)
          clients = SAMPList (map SAMPString (hubNames : clNames))
          selClient cd = if cdSecret cd == secret
                         then Nothing
                         else Just (fromClientName (cdName cd))

      dbgIO ("Found registered clients other than client " ++
             show (cdName sender))
      return (SAMPReturn clients)


getSubscribedClients ::
    HubInfo
    -> ClientSecret
    -> MType         -- ^ does not include a wildcard
    -> IO SAMPMethodResponse
getSubscribedClients hi secret msg =
    withClient hi secret $ \hub sender -> do
      let clMap = hiClients hub

          procSubs clSubs = 
            let matches = lookupSubMap msg clSubs
                -- if there are multiple matches, pick the most-specific
                -- match, which is taken to be the one with the
                -- longest mtype (this seems unlikely to happen,
                -- and the SAMP 1.3 standard doesn't say what should
                -- happen here).
                getLen = length . fromMType
                lenMatches = map (first getLen) matches
                invCompare = flip compare
                ordMatches = sortBy (invCompare `on` fst) lenMatches
            in case ordMatches of
                 [] -> Nothing
                 ((_, kvs) : _) -> return (SAMPMap kvs)
                   
          getSubs cd = do
            guard (cdSecret cd /= secret)
            matchSubs <- procSubs (cdSubscribed cd)
            return (fromClientName (cdName cd), matchSubs)
    
          -- the hub isn't going to be making this request, so
          -- do not need to include a name check here
          getHubSubs = do
            let hir = hiReader hi
            matchSubs <- procSubs (hiSubscribed hir)
            return (hiName hir, matchSubs)

          subList = catMaybes (getHubSubs : map getSubs (M.elems clMap))
          subs = M.fromList subList
               
      dbgIO ("Found subscribed clients other than client " ++
             show (cdName sender))
      return (makeResponse subs)


extractData ::
    HubInfo
    -> ClientName
    -> ClientSecret
    -> (HubInfoReader -> a)
    -> (ClientData -> a)
    -> (a -> IO SAMPMethodResponse)
       -- ^ this is applied to the first match form the hub or list of
       --   clients
    -> IO SAMPMethodResponse
extractData hi name secret hubQuery clQuery act =
    let doit hub _ = do
          let clientmap = hiClients hub
              hir = hiReader hi
              find cd = cdName cd == name
              mquery = if hiName hir == fromClientName name
                       then [hubQuery hir]
                       else map clQuery (filter find (M.elems clientmap))

          case mquery of
            (query:_) -> act query

            _ -> reportError ("Invalid client id: " ++ show name)

    in withClient hi secret doit

       
getMetadata ::
    HubInfo
    -> ClientSecret
    -> ClientName    -- ^ the client whose metadata is being requested
    -> IO SAMPMethodResponse
getMetadata hi secret clName =
    let act query = do
          dbgIO ("Found a match for client: " ++ show clName)
          return (makeResponse query)
    in extractData hi clName secret hiMetadata cdMetadata act

       
getSubscriptions ::
    HubInfo
    -> ClientSecret
    -> ClientName    -- ^ the client whose subscriptions is being requested
    -> IO SAMPMethodResponse
getSubscriptions hi secret clName = 
    let act query = let svals = fromSubMap query
                        conv (k, kvs) = (fromMTypeRS k, SAMPMap kvs)
                        rmap = M.fromList (map conv svals)
                    in do
                      dbgIO ("Found a match for client: " ++ show clName)
                      return (makeResponse rmap)
    in extractData hi clName secret hiSubscribed cdSubscribed act


-- experimental JSON responses

jsonGetClients ::
    HubInfo
    -> IO (M.Map String [String])
jsonGetClients hi =
  withHubP hi $ \hub ->
    let clMap = hiClients hub
        getName = fromClientName . cdName
        out = hiName (hiReader hi) : map getName (M.elems clMap)
        names = map fromRString out
    in M.singleton "clients" names


-- | Return the metadata. The outer map has the key
--   "metadata". Its value is a map with the client
--   name as the key and the value is the metadata.
jsonGetMetadata ::
    HubInfo
    -> IO (M.Map String (M.Map String (M.Map String SAMPValue)))
jsonGetMetadata hi =
    withHubP hi $ \hub -> 
      let hir = hiReader hi
          clMap = hiClients hub

          -- Leaving the return as MetadataMap rather than Map String SAMPValue
          -- seemed ot lead to it being treated as [SAMPValue] instead - i.e.
          -- the keys were lost (or I was doing something very stupid)
          mapKey = M.mapKeys fromRString
          conv cd = (fromClientName (cdName cd),
                     mapKey (cdMetadata cd))

          mdata = (hiName hir, mapKey (hiMetadata hir)) :
                         map conv (M.elems clMap)

          converted = map (first fromRString) mdata
                  
      in M.singleton "metadata" (M.fromList converted)


-- | Return the subscriptions. The outer map has the key
--   "subscriptions". Its value is a map with the client
--   name as the key and the value is the subscription
--   info, which is itself a map from message name to
--   a map. Although now there's an extra map that I don't understand!
jsonGetSubscriptions ::
    HubInfo
    -> IO (M.Map String (M.Map String (M.Map String (M.Map String J.Value))))
       -- ^ The returned object has the key \"subscriptions". The
       --   contents are labelled with the client name, each of
       --   which lists the messages being subscribed to, with the value
       --   being the (as-yet-unspecified) object associated with the
       --   subscription.
jsonGetSubscriptions hi =
    withHubP hi $ \hub -> 
      let hir = hiReader hi
          clMap = hiClients hub

          conv (mtype, ms) = (fromMType mtype, M.fromList (map c minfo))
              where
                minfo = M.toList ms
                c (k, vs) = (fromRString k, J.toJSON vs)

          convSubs smap = M.fromList (map conv (fromSubMap smap))
    
          hubSubs = (hiName hir, convSubs (hiSubscribed hir))
          toSub cd = (fromClientName (cdName cd), convSubs (cdSubscribed cd))
          out = hubSubs : map toSub (M.elems clMap)
          subs = M.fromList (map (first fromRString) out)

      in M.singleton "subscriptions" subs

-- | Return the callback endpoints of those clients which have
--   called samp.hub.setXmlrpcCallback.
--
jsonGetCallbacks ::
    HubInfo
    -> IO (M.Map String (M.Map String String))
       -- ^ The returned object has the key \"callbacks\". The
       --   contents are labelled with the client name, each of
       --   which gives the URI, including the hub.
jsonGetCallbacks hi =
    withHubP hi $ \hub ->
      let hir = hiReader hi
          clMap = hiClients hub

          fromName = fromRString . fromClientName
          conv cd = do
            cback <- cdCallback cd
            return (fromName (cdName cd), cback)

          hubInfo = (fromRString (hiName hir), hiCallback hir)
               
          out = hubInfo : mapMaybe conv (M.elems clMap)
          cbs = M.fromList out

      in M.singleton "callbacks" cbs
