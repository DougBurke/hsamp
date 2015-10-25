{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2015
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

  - add web profile

  - work out what's going on with ds9 connections

  - could run connections to clients using async, storing the
    thread ids, so that they can be stopped if the client shut
    downs (or hub or ...)

  - removal of problematic clients

  - pinging an existing hub to see if we can take over

  - handling of x-samp messages

  - clean up and refactoring, e.g. 
    - replace MType with separate "complete" and "allows wildcard"
      versions
    - redo SAMPResponse so that it can handle extra values

-}

module Main where

import qualified Control.Exception as CE

import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
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
import Control.Monad (forM, forM_, guard, unless, void, when)
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

import qualified Network.XmlRpc.Internals as XI

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

-- Hack for ds9 7.3.2 support
import qualified HubCall as HC
    
-- import Paths_hsamp (version)

-- the name of the SAMP logging instance
cLogger :: String
cLogger = "SAMP.StandardProfile.Hub"

-- log the message to the SAMP logger at the debug or info level.
--
-- why did I move away from the MonadIO version?
-- dbg :: (MonadIO m) => String -> m ()

dbg, info, notice :: String -> ActionM ()
dbg = liftIO . dbgIO
info = liftIO . infoIO
notice = liftIO . noticeIO

dbgIO, infoIO, noticeIO :: String -> IO ()
dbgIO = debugM cLogger . ("DBG " ++)
infoIO = infoM cLogger . ("INFO " ++)
noticeIO = noticeM cLogger . ("NOTICE " ++)
           
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

homeEnvVar :: String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
homeEnvVar = "USERPROFILE"
#else
homeEnvVar = "HOME"
#endif

err :: String -> IO ()
err msg = hPutStrLn stderr ("ERROR: " ++ msg) >> exitFailure

{- | Return the location of the lock file. If the file exists then
     exit with an error. Note that this always uses $HOME/.samp

     This has not been tested on Windows so will probably fail there.
     It does not use Haskell's System.Directory.getHomeDirectory since
     this is unliekly to give the correct location on Windows.

TODO: need to check that an existing hub responds to a samp.hub.ping call

-}
getLockFile :: IO String
getLockFile = do
    home <- getEnv homeEnvVar  -- raises an exception if var not found
    let fname = "file://" ++ home ++ "/.samp"
        furl = parseAbsoluteURI fname
    case furl of
        Just url -> do
            let hubName = uriPath url
            flag <- doesFileExist hubName
            if flag
              then err ("Is a SAMP hub already running? " ++ hubName ++
                        " already exists.") >> return ""
              else return hubName

        _ -> err ("Unable to parse location: <" ++ fname ++ ">") >> return ""

-- | Create the hub file. The file is set to have no permissions
--   in the group or owner settings (this makes the routine non-portable).
--
--   It is a bit ugly; is there an easier way to do this?
writeLockFile :: String -> String -> RString -> IO ()
writeLockFile name url secret = do
    let cts = [ "# TEST SAMP hub, using the Haskell SAMP library"
              , "samp.secret=" ++ fromRString secret
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
--   Note that, at present, there is a lot of repeated work
--   here and in the caller (i.e. checking that the receiver is
--   known). This needs to be consolidated once the sendToAll processing
--   has been written.
--
newMessageId ::
    HubInfo
    -> ClientSecret -- ^ the client which will be sent the message Id
    -> (MessageTag, ClientData, Maybe (MVar L.ByteString)) -- ^ the source of the message
    -> IO (Either String MessageId)
       -- ^ in case the client can not be found
newMessageId hi secret (tag, sender, mmvar) = do
  let mvar = hiState hi
  ohub <- takeMVar mvar
  let oclMap = hiClients ohub
  case M.lookup secret oclMap of
    Just orec -> do

      cTime <- getCurrentTime
               
      let oflight = cdInFlight orec
          onum = cdNextInFlight orec
          nnum = succ onum

          ifd = IFD { ifdSecret = cdSecret sender
                      -- if get to here the sender must have a callback
                    , ifdUrl = fromJust (cdCallback sender)
                    , ifdTag = tag
                    , ifdMVar = mmvar
                    , ifdTime = cTime }
          
          -- Inserting a prefix to ensure no collision is
          -- rather OTT here, given that the likelihood of
          -- collision is small. However, it may make manual
          -- tracing of message flow a bit easier.
          (s, ngen) = makeSecret (hiRandomGen ohub)
          mid = toMessageId (fromJust (toRString ("mid" ++ show onum ++ ":" ++
                                                  fromRString s)))

          nflight = M.insert mid ifd oflight
          nrec = orec { cdInFlight = nflight
                      , cdNextInFlight = nnum }
          nclMap = M.adjust (const nrec) secret oclMap
          nhub = ohub { hiClients = nclMap
                      , hiRandomGen = ngen }

      putMVar mvar nhub
      dbgIO ("Inserted messageId=" ++ show mid ++ " for receiver=" ++
            fromRString (fromClientName (cdName orec)))
      nclMap `seq` nhub `seq` nflight `seq` ifd `seq`
             mid `seq` nnum `seq` return ()
      return (Right mid)
            
    _ -> do
      dbgIO ("Unable to find the receiver matching secret=" ++
             fromRString (fromClientSecret secret))
      return (Left "Unknown receiving client")


-- TODO: clean up repeated code
removeMessageId ::
    HubInfo
    -> MessageId     -- ^ message to remove
    -> ClientSecret  -- ^ secret of client that uses the message
                     --   (i.e. the one that uses this id); this is not
                     --   needed, but it looks like we have it so can
                     --   avoid some work here
    -> IO ()
removeMessageId hi mid secret = do
  let mvar = hiState hi
  ohub <- takeMVar mvar
  let oclMap = hiClients ohub
  case M.lookup secret oclMap of
    Just ocd -> do
      let oflMap = cdInFlight ocd
          nflMap = M.delete mid oflMap
          ncd = ocd { cdInFlight = nflMap }
          nclMap = M.adjust (const ncd) secret oclMap
          nhub = ohub { hiClients = nclMap }
      dbgIO ("Removing message id " ++ show mid ++ " client " ++
             show (cdName ocd))
      putMVar mvar nhub
      nhub `seq` nclMap `seq` ncd `seq` nclMap `seq` return ()
           
    Nothing -> do
      dbgIO ("Looks like the client for message id " ++ show mid ++
             " has already been removed")
      putMVar mvar ohub

              
setupHubFile :: IO (Socket, FilePath, RString)
setupHubFile = do
    lockfile <- getLockFile
    putStrLn ("Using lock file: " ++ lockfile)

    -- what socket is going to be used?
    sock <- getSocket
    port <- socketPort sock
    putStrLn ("Using port " ++ show port ++ " for the SAMP hub.")

    gen <- getStdGen
    let (secret, gen') = makeSecret gen
    setStdGen gen'

    -- as we know the port calling getHubURL is excessive here, but
    -- a bit simpler
    -- let xmlrpc = "http://127.0.0.1:" ++ show port ++ "/xmlrpc"
    huburl <- getHubURL sock
    let xmlrpc = huburl ++ "xmlrpc"
                 
    writeLockFile lockfile xmlrpc secret
    return (sock, lockfile, secret)

           
getHubURL :: Socket -> IO String
getHubURL sock = do
  port <- socketPort sock
  return ("http://127.0.0.1:" ++ show port ++ "/")

         
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
  hub <- readMVar (hiState hi)
  forM_ (M.elems (hiClients hub))
        (broadcastRemovedClient hi . cdName)
  broadcastMType hi name "samp.hub.event.shutdown" []
                 

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
    -> MType          -- ^ the message to send
    -> [SAMPKeyValue] -- ^ the arguments for the message
    -> [SAMPKeyValue] -- ^ extra arguments
    -> ActionM ()
notify hi secret recName msg args otherArgs = do
  let hir = hiReader hi
      mvar = hiState hi

  hub <- liftIO (readMVar mvar)

  let clMap = hiClients hub
      msender = M.lookup secret clMap

      mreceiver = findSubscribedClient clMap msg recName

      -- for the hub check, can drop the callable requirement
      hubMatch = hiName hir == fromClientName recName &&
                 memberSubMap msg (hiSubscribed hir)

      -- receiver is a client, and not the hub
      send sendName receiver = do
        info ("Sending " ++ fromMType msg ++ " from " ++
              show sendName ++ " to " ++ show recName)
        forkCall (notifyRecipient sendName receiver msg args otherArgs)
        return HC.emptyResponse

      -- The receiver is the hub, so do not need to send a message,
      -- but do we need to do something? At this point we know the
      -- hub is subscribed to the message
      sendHub _ = do
        info ("Hub has been notified about " ++ show msg)
        return HC.emptyResponse

      noReceiver = do
        info "No match to the notify message"
        let emsg = "The receiver is not callable or not subscribed to " ++
                   fromMType msg
        return (errorResponse emsg)

      noSender = do
        info ("Unable to find a client with secret=" ++ show secret)
        return (errorResponse "Invalid client id")
          
  rsp <- case msender of
           Just sender ->
               let sendName = fromClientName (cdName sender)
               in if hubMatch
                  then sendHub sendName
                  else case mreceiver of
                         Just receiver -> send sendName receiver
                         Nothing -> noReceiver
               
           Nothing -> noSender
                    
  respond "notify" rsp

        
-- | A client wants to send a message to all the interested clients
--   via the notify system.
--
notifyAll ::
    HubInfo
    -> ClientSecret   -- ^ the client sending the message
    -> MType          -- ^ the message to send
    -> [SAMPKeyValue] -- ^ the arguments for the message
    -> [SAMPKeyValue] -- ^ extra arguments
    -> ActionM ()
notifyAll hi secret msg args otherArgs = do
  let hubName = hiName hir
      hir = hiReader hi
      mvar = hiState hi

  hub <- liftIO (readMVar mvar)

  let clMap = hiClients hub
      msender = M.lookup secret clMap

      -- do not need to worry about callable for the hub
      hubMatch = memberSubMap msg (hiSubscribed hir)

      sendClient sendName receiver = do
        let recName = cdName receiver
        info ("Sending " ++ fromMType msg ++ " from " ++ fromRString sendName
              ++ " to " ++ show recName)
        forkCall (notifyRecipient sendName receiver msg args otherArgs)
                                 
      sendHub _ = info ("Hub has been notified about " ++ show msg)

  rsp <- case msender of
           Just sender -> do
             let sendName = fromClientName (cdName sender)
                 receivers = findOtherSubscribedClients clMap (cdName sender) msg
                  
             forM_ receivers (sendClient sendName)
             when hubMatch (sendHub sendName)
                   
             let rNames = map (toSValue . cdName) receivers
                 hName = SAMPString hubName
                 names = if hubMatch then hName : rNames else rNames
                 rval = SAMPReturn (SAMPList names)
             return (renderSAMPResponse rval)

           Nothing -> do
             info ("Unable to find a client with secret=" ++ show secret)
             return (errorResponse "Invalid client id")
                    
  respond "notifyAll" rsp


{-

Could the MVar in sendToClient - currently used for callAndWait - be
used for all cases. Could attach a timer thread to it, that will
"cancel" the MVar so can identify problematic systems?
-}

-- TODO: findSubscribedClients to include the hub?

-- | Return the callables clients that are subscribed to this message.
findSubscribedClients ::
    ClientMap
    -> MType
    -> [ClientData]
findSubscribedClients clMap msg =
    let find cd = isJust (cdCallback cd) &&
                  memberSubMap msg (cdSubscribed cd)
    in M.elems (M.filter find clMap)

findOtherSubscribedClients ::
    ClientMap
    -> ClientName -- ^ client to exclude from the search
    -> MType
    -> [ClientData]
findOtherSubscribedClients clMap clName msg =
    let find cd = cdName cd /= clName &&
                  isJust (cdCallback cd) &&
                  memberSubMap msg (cdSubscribed cd)
    in M.elems (M.filter find clMap)

-- | Return the client if it is callable and subscribed to the message.
--
--   See also findSubscribedClientE.
findSubscribedClient ::
    ClientMap
    -> MType
    -> ClientName
    -> Maybe ClientData
findSubscribedClient clMap msg clName =
    let isClient cd = cdName cd == clName
    in listToMaybe (filter isClient (findSubscribedClients clMap msg))

-- | Return the client if it is callable and subscribed to the message.
--
--   See also findSubscribedClient.
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
                        else Left ("Client " ++ nameStr ++ " is not " ++
                                   "subscribed to " ++ show msg)
                   else Left ("Client " ++ nameStr ++ " is not callable")
         _ -> Left ("There is no client called " ++ nameStr)

       
-- Check if the hub is subscribed to the message and is not the
-- sender
isHubSubscribed :: HubInfo -> MType -> ClientName -> Bool
isHubSubscribed hi msg sendName =
    let hir = hiReader hi
    in hiName hir /= fromClientName sendName && memberSubMap msg (hiSubscribed hir)

           
-- | A client wants to send a message to another client via the
--   call system.
--
--   TODO: does this need to cope with the receiver being the hub?
sendToClient ::
    HubInfo
    -> ClientSecret   -- ^ the client sending the message
    -> ClientName     -- ^ the client to send the message to
    -> MessageTag
    -> MType          -- ^ the message to send
    -> [SAMPKeyValue] -- ^ the arguments for the message
    -> [SAMPKeyValue] -- ^ extra arguments
    -> ActionM ()
sendToClient hi secret recName tag msg args otherArgs = do

  -- lots of repeated code with newMessageId; resolve once sendToAll
  -- message is handled
   
  msender <- liftIO (findCallableClient hi secret)

  -- TODO: should only read hub if msender succeeds (and repeated calls
  -- to the MVar here is not ideal).
  let mvar = hiState hi
  hub <- liftIO (readMVar mvar)

  let clMap = hiClients hub

      mRec = findSubscribedClient clMap msg recName
              
      noReceiver = do
        info "Receiver is not subscribed to the message (or is unknown)"
        return (errorResponse ("receiver is unknown, not callable, or not " ++
                               "subscribed to " ++ fromMType msg))

      sendTo sender receiver = do
        let rSecret = cdSecret receiver
        emid <- liftIO (newMessageId hi rSecret (tag,sender,Nothing))
        case emid of
          Right mid -> do
            let sendName = cdName sender
            info ("Sending " ++ fromMType msg ++ " from " ++
                  show sendName ++ " to " ++ show recName)
            forkCall (clientReceiveCall sendName receiver mid msg args otherArgs)
            return (renderSAMPResponse (SAMPReturn (toSValue mid)))

          Left _ -> do
            info "*** Looks like the receiver has disappeared ***"
            return (errorResponse "receiver is no longer available")
                                   
  rsp <- case msender of
           Right sender ->
               case mRec of
                 Just rcd -> sendTo sender rcd
                 _ -> noReceiver
                    
           Left emsg -> do
             dbg emsg
             return (errorResponse emsg)
                    
  respond "call" rsp


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
    -> MType          -- ^ the message to send
    -> [SAMPKeyValue] -- ^ the arguments for the message
    -> [SAMPKeyValue] -- ^ extra arguments
    -> ActionM ()
sendToAll hi secret tag msg args otherArgs = do

  -- TODO: will need to review and clean up this code
  
  let mvar = hiState hi
            
  hub <- liftIO (readMVar mvar)

  let clMap = hiClients hub
      msender = M.lookup secret clMap

  rsp <- case msender of
           Just sender -> do

               let sendName = cdName sender
                   receivers = findOtherSubscribedClients clMap sendName msg

                   isHub = isHubSubscribed hi msg sendName

               let recNames = unwords (map (show . cdName) receivers)
               dbg ("sendToAll with sendName= " ++ show sendName ++
                    " recNames=" ++ recNames)
                        
               mkvs <- forM receivers $ \rcd -> do
                                       
                   let rSecret = cdSecret rcd
                       rName = cdName rcd

                   dbg (" - sendToAll to " ++ show rName)
                               
                   emid <- liftIO (newMessageId hi rSecret (tag,sender,Nothing))
                   case emid of
                     Right mid -> do
                         dbg ("Sending " ++ fromMType msg ++ " from " ++
                              show sendName)

                         forkCall (clientReceiveCall sendName rcd mid msg args otherArgs)
                         return (Just (fromClientName rName, toSValue mid))

                     Left _ -> do
                         dbg ("Unable to create message for " ++ show rName)
                         return Nothing

               hubAns <- if isHub
                         then liftIO (hubReceiveCall hi tag sendName msg args)
                         else return Nothing
                               
               let kvs = catMaybes (hubAns : mkvs)
               return (renderSAMPResponse (SAMPReturn (SAMPMap kvs)))
                       
           Nothing -> do
             info ("Unable to find a client with secret=" ++ show secret)
             return (errorResponse "Invalid client id")
                    
  respond "callAll" rsp


-- | The hub has been contacted with a request for data. If got
--   this far then the hub is subscribed (this check could be moved
--   within this routine), and I assume that the client is callable.
--
hubReceiveCall ::
    HubInfo
    -> MessageTag    -- ^ tag from the client
    -> ClientName    -- ^ client making the request
    -> MType
    -> [SAMPKeyValue]
    -> IO (Maybe (RString, SAMPValue))
    -- ^ The hub name and the message id (as a SAMPValue)
hubReceiveCall hi mtag sendName msg args = do
  let mvar = hiState hi
  ohub <- takeMVar mvar
  let ogen = hiRandomGen ohub
      (s, ngen) = makeSecret ogen
      nhub = ohub { hiRandomGen = ngen}
      -- note: unlike clients, do not include a counter in the
      --       message identifier
      mid = toMessageId s

  putMVar mvar nhub
  -- NOTE: we do not actually use the mid argument in sendToHub!
  _ <- forkIO (sendToHub hi mtag sendName msg args [])
  
  return (Just (hiName (hiReader hi), toSValue mid))


-- | The hub has been sent a message as a SAMP client. It is
--   therefore one of the messages that the hub is subscribed
--   to.
--
sendToHub ::
    HubInfo
    -> MessageTag
    -> ClientName  -- perhaps the secret should be sent instead?
    -> MType
    -> [SAMPKeyValue]
    -> [SAMPKeyValue]  -- ^ extra arguments
    -> IO ()
sendToHub hi mtag sendName msg args otherArgs = do
  let mvar = hiState hi
  ohub <- readMVar mvar
  let clMap = hiClients ohub

      -- add in callable check, even though it should be guaranteed
      -- at this point.
      find cd = cdName cd == sendName && isJust (cdCallback cd)
      msender = listToMaybe (M.elems (M.filter find clMap))

  dbgIO ("Hub sent " ++ fromMType msg ++ " by " ++ show sendName)
  case msender of
    Just sender -> do
      -- hub has to send a message back to the sender using the
      -- message tag
      --
      replyData <- hubProcessMessage hi msg args otherArgs -- ?
      let url = fromJust (cdCallback sender)
          secret = cdSecret sender
          
          rmap = toSValue replyData
          arglist = [ toSValue secret
                    , toSValue (hiName (hiReader hi))
                    , toSValue mtag
                    , rmap ]
          nargs = map XI.toValue arglist

          act = void (HC.call url "samp.client.receiveResponse" nargs)
          hdl e = dbgIO ("Error in sending reply to the original client at " ++
                         url ++ " - " ++ e)

          ignoreError :: CE.SomeException -> IO ()
          ignoreError e = do
            hPutStrLn stderr "*** Error notifying original client"
            hPutStrLn stderr ("*** " ++ show e)

      dbgIO "Sending receiveResponse from hub"
      handleError hdl act
                  `CE.catch` ignoreError

    Nothing -> do
      dbgIO "--> looks like the client has closed down; hub no-op."
      return ()
                

-- | The hub has been sent a message it is subscribed to.
--
hubProcessMessage ::
    HubInfo
    -> MType
    -> [SAMPKeyValue]
    -> [SAMPKeyValue] -- ^ extra arguments
    -> IO SAMPResponse
hubProcessMessage hi mtype args otherArgs = do
  let funcs = hiHandlers (hiReader hi)
      emsg = "Internal error: hub is not subscribed to " ++ mtToRS mtype
  case lookup mtype funcs of
    Just f -> f hi mtype args otherArgs
    _ -> return (toSAMPResponseError emsg [])

        
-- | An empty SAMP Response
emptySAMPResponse :: SAMPResponse
emptySAMPResponse = toSAMPResponse []

                    
-- | Process the samp.app.ping message sent to the hub
hubSentPing :: HubHandlerFunc
hubSentPing _ _ args otherArgs = do
  infoIO "Hub has been sent samp.app.ping"
  infoIO ("params = " ++ show args)
  infoIO (" extra = " ++ show otherArgs)
  return (toSAMPResponse otherArgs) -- emptySAMPResponse TODO: is this correct

{-
-- | Process the [x-]samp.query.by-meta message sent to the hub.
hubSentQueryByMeta :: HubHandlerFunc                 
hubSentQueryByMeta _ _ _ = return (toSAMPResponse [])
-}

toDuration :: Int -> Integer
toDuration = toInteger . (1000000 *)

sleep :: Integer -> IO ()
sleep t | t <= 0    = return ()
        | otherwise =
            let sleepMax = maxBound :: Int
                (niter, left) = t `quotRem` (toInteger sleepMax)
                go n | n <= 0    = threadDelay (fromInteger left)
                     | otherwise = threadDelay sleepMax >> go (n-1)
            in go niter
      
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
    -> MType          -- ^ the message to send
    -> [SAMPKeyValue] -- ^ the arguments for the message
    -> [SAMPKeyValue] -- ^ extra arguments
    -> ActionM ()
callAndWait hi secret recName waitTime msg args otherArgs = do

  -- lots of repeated code with sendToClient

  let act sender = do
        dbg ("Processing callAndWait from " ++ show (cdName sender) ++
             "to " ++ show recName)

        if fromClientName recName == hiName (hiReader hi)
          then do
            -- no timeout for hub connections!
            replyData <- liftIO (hubProcessMessage hi msg args otherArgs)
            let rmap = toSValue replyData
                rsp = renderSAMPResponse (SAMPReturn rmap)
            return rsp
            
          else do
            let mvar = hiState hi
            hub <- liftIO (readMVar mvar)

            let clMap = hiClients hub
                mRec = findSubscribedClientE clMap msg recName

            case mRec of
                 Right rcd -> sendTo sender rcd
                 Left emsg -> noReceiver emsg
               
      sendTo sender receiver = do
        let rSecret = cdSecret receiver
        rspmvar <- liftIO newEmptyMVar
        emid <- liftIO (newMessageId hi rSecret (dummyTag,sender,Just rspmvar))
        case emid of
          Right mid -> do
            let sendName = cdName sender
                errorRsp = rawError "Timeout"
                           
            dbg ("Sending " ++ fromMType msg ++ " from " ++
                 show sendName ++ " to " ++ show recName)

            -- Note: the timer starts at receipt of the message
            --       to the client, not from when the 
            --       samp.client.receiveCall is made
            let doit = clientReceiveCall sendName receiver mid msg args
                                         otherArgs
            forkCall doit

            let tVal = toDuration waitTime
            when (tVal > 0) (
                forkCall (sleep tVal
                          >> putMVar rspmvar errorRsp
                          >> removeMessageId hi mid rSecret)
                )

            liftIO (takeMVar rspmvar)

          Left _ -> do
            dbg "*** Looks like the receiver has disappeared ***"
            return (rawError "receiver is no longer available")

      noReceiver emsg = do
        dbg emsg
        return (rawError emsg)

  -- NOTE: for calLAndWait the client does not need to be callable!
  msender <- liftIO (findClient hi secret)
  rsp <- case msender of
           Just sender -> act sender
                        
           Nothing -> dbg "Unable to identify client"
                      >> return (errorResponse "Invalid client id")

  respond "callAndWait" rsp
             
             
-- | A client is replying to a message from another client.
--
--   TODO: error out if the client sending the message is not callable.
--
reply ::
    HubInfo
    -> ClientSecret   -- ^ the client sending the message
    -> MessageId
    -- -> SAMPResponse    -- ^ response to the message
    -> [SAMPKeyValue]    -- ^ response to the message
    -> ActionM ()
reply hi secret msgId replyData = do

  -- lots of repeated code
  
  let mvar = hiState hi

  hub <- liftIO (readMVar mvar)

  let clMap = hiClients hub
      msender = M.lookup secret clMap

      -- check for message tag:
      -- If there is an id in the map, then at present the
      -- client should be callable (since do not allow the
      -- callback field to be unset), but enforce the
      -- check.
      --
      find cd = isJust (cdCallback cd) && M.member msgId (cdInFlight cd)
      recMap = M.filter find clMap

  rsp <- case msender of
           Just _ ->
               -- TODO: could use sender here to get at info
               -- that is in the InFlightData item.
               --
               -- assume only one match so just use the first one
               case M.elems recMap of
                 (cd:_) -> do
                     let Just ifd = M.lookup msgId (cdInFlight cd)
                     dbg ("Replying to client with secret=" ++
                          show (ifdSecret ifd))
                     forkCall (replyToOrigin (cdName cd) ifd replyData)
                     liftIO (removeMessageId hi msgId (cdSecret cd))
                     return HC.emptyResponse
                           
                 _ -> do
                   -- is this considered an error?
                   dbg ("No recepient for message-id: " ++ show msgId)
                   return (errorResponse "Originating client no-longer exists")
           Nothing -> do
             dbg ("Unable to find a client with secret=" ++ show secret)
             return (errorResponse "Invalid client id")
                    
  respond "reply" rsp


-- | Send a message back to the client that started the call/response
--   query. This is complicated by the way I am handling callAndWait
--   calls, where the response should be placed in the MVar rather
--   then sent directly.
--
replyToOrigin ::
    ClientName       -- ^ name of the client making the response
    -> InFlightData
    -- -> SAMPResponse
    -> [SAMPKeyValue]
    -> IO ()
replyToOrigin clName ifd replyData =
    let url = ifdUrl ifd
        secret = ifdSecret ifd
        tag = ifdTag ifd

        -- rmap = toSValue replyData
        rmap = SAMPMap replyData
        arglist = [ SAMPString (fromClientSecret secret)
                  , SAMPString (fromClientName clName)
                  , SAMPString (fromMessageTag tag)
                  , rmap ]
        nargs = map XI.toValue arglist

        act = void (HC.call url "samp.client.receiveResponse" nargs)
        hdl e = dbgIO ("Error in sending reply to the original client at " ++
                       url ++ " - " ++ e)

        ignoreError :: CE.SomeException -> IO ()
        ignoreError e = do
          hPutStrLn stderr "*** Error notifying original client"
          hPutStrLn stderr ("*** " ++ show e)

        rsp = renderSAMPResponse (SAMPReturn rmap)
                    
    in case ifdMVar ifd of
         Just mvar -> putMVar mvar rsp
         _ -> do
           dbgIO ("Sending receiveResponse from " ++ show clName)
           handleError hdl act
              `CE.catch` ignoreError


-- | Notify a client about a message.
--
--   TODO: should this note down problematic clients so that they
--         can get removed (or something else done to them?)
--
--   TODO: should just be sent the ClientData structure, rather than
--         name/secret/url (and if not callable it's a nop)
--
notifyRecipient ::
    RString   -- ^ the name of the client sending the notification
    -> ClientData -- ^ the recipient; if it is not callable then this
                  --   does nothing.
    -> MType          -- ^ the message to notify recipient about
    -> [SAMPKeyValue] -- ^ arguments
    -> [SAMPKeyValue] -- ^ extra arguments
    -> IO ()
       -- ^ any error from the call is logged (debug level)
       --   but otherwise ignored
notifyRecipient sendName receiver mtype args otherArgs = do
    let recName = fromRString (fromClientName (cdName receiver))
        recSecret = cdSecret receiver

        msg = mtToRS mtype
        -- must have some conversion routines for this?
        argmap = [ ("samp.mtype", SAMPString msg)
                 , ("samp.params", SAMPMap args) ]
                 ++ otherArgs
        arglist = [ toSValue recSecret
                  , SAMPString sendName
                  , SAMPMap argmap ]
        nargs = map XI.toValue arglist
        act url = void (HC.call url "samp.client.receiveNotification" nargs)
        hdl url e = dbgIO ("Error in sending " ++ show mtype ++ " to " ++
                           url ++ " - " ++ e)

        ignoreError :: CE.SomeException -> IO ()
        ignoreError e = do
          hPutStrLn stderr ("** Error notifying client " ++ recName ++
                            " about message " ++ fromMType mtype)
          hPutStrLn stderr ("** " ++ show e)

    case cdCallback receiver of
      Just url -> do
        dbgIO ("Sending notify of " ++ fromMType mtype ++ " to client " ++
               recName)
        handleError (hdl url) (act url)
                        `CE.catch` ignoreError

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
    -> MType -- ^ the message to send
    -> [SAMPKeyValue] -- ^ arguments
    -> [SAMPKeyValue] -- ^ extra arguments 
    -> IO ()
       -- ^ any error from the call is logged (debug level)
       --   but otherwise ignored
clientReceiveCall sendName receiver msgId mtype args otherArgs = 
    let rSecret = cdSecret receiver
        rName = fromRString (fromClientName (cdName receiver))
                
        msg = mtToRS mtype
        -- must have some conversion routines for this?
        argmap = [ ("samp.mtype", SAMPString msg)
                 , ("samp.params", SAMPMap args) ]
                 ++ otherArgs
        arglist = [ toSValue rSecret
                  , toSValue sendName
                  , toSValue msgId
                  , SAMPMap argmap ]
        nargs = map XI.toValue arglist
        hdl url e = dbgIO ("Error in sending " ++ show mtype ++ " to " ++
                           url ++ " - " ++ e)

        ignoreError :: CE.SomeException -> IO ()
        ignoreError e = do
          hPutStrLn stderr ("** Error notifying client " ++ rName ++
                            " about message " ++ fromMType mtype)
          hPutStrLn stderr ("** " ++ show e)

        act url = void (HC.call url "samp.client.receiveCall" nargs)

    in case cdCallback receiver of
         Just url -> do
           dbgIO ("Sending call of " ++ fromMType mtype ++ " to client " ++
                  rName)
           handleError (hdl url) (act url)
                           `CE.catch` ignoreError

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

                                             
createHub :: Socket -> HubInfo -> RString -> IO ()
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

callFromClient :: HubInfo -> RString -> ActionM ()
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
         
-- For now, there's no way to indicate a failure, which is not ideal.
-- Probably want to re-work the code
toReturnValue :: SAMPResponse -> L.ByteString
toReturnValue = XI.renderResponse . XI.Return . XI.toValue

handleSAMP ::
    HubInfo
    -> RString
    -> SAMPMethodCall
    -> ActionM ()
handleSAMP hi secret (SAMPMethodCall name args) = do
  liftIO (dumpHub hi)
  let mname = fromRString name
  dbg ("Server called with method: " ++ mname)
  dbg ("  args: " ++ show args)

  case lookupMType hi mname of
    Just f -> f hi secret args
    _ -> respondError ("Unsupported message: " ++ mname)

-- The XML-RPC standard mandates that both the Content-Type and
-- Content-Length headers are included in the response. This is
-- probably handled by haxr's server code, but I am not using that here.
respond :: String -> L.ByteString -> ActionM ()
respond lbl rsp = do
  dbg ("Response [" ++ lbl ++ "] " ++ L.unpack rsp)
  setHeader cType "text/xml"
  setHeader cLength (toLen rsp)
  raw rsp

respondToClient :: String -> IO L.ByteString -> ActionM ()
respondToClient lbl act = liftIO act >>= respond lbl
      
-- An empty response
respondOkay :: HubFunc
respondOkay _ _ _ = respond "okay" (toReturnValue emptySAMPResponse)

-- Errors can be returned as a SAMP Response - e.g.
--     toSAMPResponseError msg []
-- but it looks like most (hopefully all) cases that respondError
-- is used should return an XML-RPC fault.
--
respondError :: String -> ActionM ()
respondError = respond "error" . rawError

rawError :: String -> L.ByteString
rawError = XI.renderResponse . XI.Fault 1
                   
handleRegister :: HubFunc
handleRegister _ _ [] = respondError "Missing hub secret"
handleRegister hi secret (SAMPString s:_)
    | secret == s = register hi
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
sampToSubMap :: [SAMPKeyValue] -> Either String SubscriptionMap
sampToSubMap kvs = 
    let conv :: SAMPKeyValue -> Either String (MType, [SAMPKeyValue])
        conv (mmt, vals) = do
          mt <- handleError Left (toMTypeE (fromRString mmt))
          case vals of
            SAMPMap xs -> Right (mt, xs)
            _ -> Left (fromRString mmt ++ " was not sent map")

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
handleNotify hi _ (SAMPString s : SAMPString rid : SAMPMap msgMap : _) =
    case parseMsgResponse msgMap of
      Right (mtype, args, otherArgs) -> notify hi (toClientSecret s) (toClientName rid) mtype args otherArgs
      Left emsg -> respondError emsg
handleNotify _ _ _ = respondError "Invalid arguments"


handleNotifyAll :: HubFunc
handleNotifyAll hi _ (SAMPString s : SAMPMap msgMap : _) =
    case parseMsgResponse msgMap of
      Right (mtype, args, otherArgs) -> notifyAll hi (toClientSecret s) mtype args otherArgs
      Left emsg -> respondError emsg
handleNotifyAll _ _ _ = respondError "Invalid arguments"

                      
handleCall :: HubFunc
handleCall hi _ (SAMPString s : SAMPString rid :
                 SAMPString tagstr : SAMPMap msgMap : _) =

    let tag = toMessageTag tagstr
        secret = toClientSecret s
        clName = toClientName rid
    in case parseMsgResponse msgMap of
         Right (mtype, args, otherArgs) ->
             sendToClient hi secret clName tag mtype args otherArgs
         Left emsg -> respondError emsg

handleCall _ _ _ = respondError "Invalid arguments"

handleCallAll :: HubFunc
handleCallAll hi _ (SAMPString s : SAMPString tagstr : SAMPMap msgMap : _) =
    let tag = toMessageTag tagstr
        secret = toClientSecret s
    in case parseMsgResponse msgMap of
         Right (mtype, args, otherArgs) ->
             sendToAll hi secret tag mtype args otherArgs
         Left emsg -> respondError emsg

handleCallAll _ _ _ = respondError "Invalid arguments"


handleCallAndWait :: HubFunc
handleCallAndWait hi _ (SAMPString s : SAMPString rid : SAMPMap msgMap :
                        timeoutArg : _) =
    let secret = toClientSecret s
        clName = toClientName rid

        argsE = do
          timeoutVal <- fromSValue timeoutArg
          (mtype, args, otherArgs) <- parseMsgResponseE msgMap
          return (mtype, args, otherArgs, timeoutVal)
                 
    in case handleError Left argsE of
         Right (mtype, args, otherArgs, tout) ->
             callAndWait hi secret clName tout mtype args otherArgs
         Left emsg -> respondError emsg

handleCallAndWait _ _ _ = respondError "Invalid arguments"

                        
handleReply :: HubFunc
handleReply hi _ (SAMPString s : SAMPString msg : SAMPMap rspMap : _) =
    let msgId = toMessageId msg
        secret = toClientSecret s
{-                 
    in case handleError Left (fromSValue (SAMPMap rspMap)) of
         Right rsp -> reply hi secret msgId rsp
         Left emsg -> respondError emsg

   FOR NOW IGNORE ERROR CHECKING IN THE RESPONSE AND JUST PASS
   STRAIGHT THROUGH TO THE ORIGINAL CLIENT
    in do
      liftIO (putStrLn ("@@> reply map=" ++ show rspMap))
      case handleError Left (fromSValue (SAMPMap rspMap)) of
         Right rsp -> reply hi secret msgId rsp
         Left emsg -> respondError emsg
-}

    in reply hi secret msgId rspMap
       
handleReply _ _ _ = respondError "Invalid arguments"
                     
-- To share the same code between async and synchronous calls,
-- we need a tag for the synchronous cases. This may indicate
-- that the code should be re-written (since the handling in the
-- synchronous case can be a lot simpler than the synchronous case).
--
dummyTag :: MessageTag
dummyTag = toMessageTag (error "dummy tag value should not be used")
           
-- some helper routines

lookupE :: (Eq k, Show k) => k -> [(k, v)] -> Either String v
lookupE k kvs = case lookup k kvs of
                  Just v -> Right v
                  _ -> Left ("Unable to find key " ++ show k)

msgToMTypeE :: RString -> Either String MType
msgToMTypeE r = handleError Left (toMTypeE (fromRString r))

getMsgTypeE :: [SAMPKeyValue] -> Either String MType
getMsgTypeE kvs = do
  val <- lookupE "samp.mtype" kvs
  case val of
    SAMPString x -> msgToMTypeE x
    SAMPList _ -> Left "Expected string for samp.mtype, sent a list" 
    SAMPMap _ -> Left "Expected string for samp.mtype, sent a map" 

-- Is this needed or can it be inlined directly into getMapFromParamsE?
getMsgParamsE :: [SAMPKeyValue] -> Either String SAMPValue
getMsgParamsE = lookupE "samp.params"

getMapFromParamsE :: [SAMPKeyValue] -> Either String [SAMPKeyValue]
getMapFromParamsE kvs = do
  val <- getMsgParamsE kvs
  case val of
    SAMPMap out -> Right out
    SAMPString _ -> Left "Expected a map for samp.params, sent a string"
    SAMPList _ -> Left "Expected a map for samp.params, sent a list"

-- | Parse a SAMP message to extract the message type and parameter
--   values. Is there a version of this in the Client code?
--
--   TODO: this is essentially parsing a SAMPResponse (but with
--         support for extra key/value pairs)
--
parseMsgResponse ::
    [SAMPKeyValue]
    -> Either String (MType, [SAMPKeyValue], [SAMPKeyValue])
       -- ^ On success, returns the MType, samp.params, and then
       --   any remaining key,value pairs.
parseMsgResponse kvs = do
  mtype <- getMsgTypeE kvs
  params <- getMapFromParamsE kvs
  let wanted (k, _) = k `notElem` ["samp.mtype", "samp.params"]
  return (mtype, params, filter wanted kvs)

parseMsgResponseE ::
    Monad m
    => [SAMPKeyValue]
    -> XI.Err m (MType, [SAMPKeyValue], [SAMPKeyValue])
parseMsgResponseE kvs = either throwError return (parseMsgResponse kvs)
                        

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
newtype SubscriptionMap = SM (HM.HashMap MType [SAMPKeyValue])
    deriving (Eq, Show)

emptySubMap :: SubscriptionMap
emptySubMap = SM HM.empty

-- | This structure should be a union type, since having a MVar
--   means that some of the other fields are not needed. Not
--   worth re-factoring at this time as this is all in flux.
data InFlightData =
    IFD { ifdSecret :: ClientSecret
        , ifdUrl :: String
        , ifdTag :: MessageTag
        , ifdMVar :: Maybe (MVar L.ByteString)
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
    -> [(MType, [SAMPKeyValue])] -- ^ empty if there is no match
lookupSubMap k (SM m) =
    if isMTWildCard k
    then -- ideally this would be an impossible condition
        error ("Internal error: sent a wildcard MType: " ++ fromMType k)
    else filter (\t -> fst t == k) (HM.toList m)
                        
toSubMap :: [(MType, [SAMPKeyValue])] -> SubscriptionMap
toSubMap = SM . HM.fromList

fromSubMap :: SubscriptionMap -> [(MType, [SAMPKeyValue])]
fromSubMap (SM m) = HM.toList m

insertSubMap :: MType -> [SAMPKeyValue] -> SubscriptionMap -> SubscriptionMap
insertSubMap k v (SM m) = SM (HM.insert k v m)

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
    -> MType
    -> [SAMPKeyValue]
    -> [SAMPKeyValue]  -- ^ extra arguments
    -> IO SAMPResponse

type HubFunc =
    HubInfo
    -> RString   -- hub secret [do we really need this?]
    -> [SAMPValue]
    -> ActionM ()

       
makeHubSubscriptions :: [(MType, HubHandlerFunc)] -> SubscriptionMap
makeHubSubscriptions hdlrs =
    let emap = []
        kvs = map (\(a,_) -> (a,emap)) hdlrs
    in toSubMap kvs


lookupMType :: HubInfo -> String -> Maybe HubFunc
lookupMType hi mtype = M.lookup mtype (hiHubHandlers (hiReader hi))
                       
defaultHubHandlers :: M.Map String HubFunc
defaultHubHandlers =
    M.fromList [
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
    , hiSecret :: RString -- the secret for the hub
    , hiSecretLen :: Int    -- number of characters in the client secrets
    , hiMetadata :: MetadataMap
    , hiSubscribed :: SubscriptionMap
    -- note: two sets of handlers
    , hiHandlers :: [(MType, HubHandlerFunc)]
    , hiHubHandlers :: M.Map String HubFunc
    , hiCallback :: String  -- URL for the hub
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
    , hiState :: MVar HubInfoState
    }
                  
dumpHub :: HubInfo -> IO ()
dumpHub hi = do
  let hir = hiReader hi
      mvar = hiState hi
  his <- readMVar mvar
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
    String          -- ^ URL of the hub
    -> StdGen       -- ^ The generator to use
    -> RString      -- ^ The secret to use
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
                   {-
                 , ("samp.query.by-meta", hubSentQueryByMeta)
                 , ("x-samp.query.by-meta", hubSentQueryByMeta)
                    -}
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
            , hiCallback = huburl
            }
      his = HubInfoState {
              hiRandomGen = gen
            , hiClients = M.empty
            , hiNextClient = 1
            }
  mvar <- newMVar his
  return HubInfo { hiReader = hir, hiState = mvar }

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
    let hir = hiReader hi
        mvar = hiState hi
    ohub <- takeMVar mvar
    let (client, secret, nhub) = addClient hir ohub
    putMVar mvar nhub
    -- try to enforce strict semantics for the changes in addClient,
    -- which is ugly and prone to my mis-understandings of things
    hiRandomGen nhub `seq`
      hiClients nhub `seq`
      hiNextClient nhub `seq`
      secret `seq`
      return ()
    return (client, secret)

-- TODO: in the broadcast code; should we identify problematic
-- clients (i.e. those that don't respond) so that they can be
-- removed?

broadcastMType ::
    HubInfo
    -> ClientName  -- ^ the client that sent the message
    -> MType       -- ^ the message
    -> [SAMPKeyValue] -- ^ arguments
    -- -> [SAMPKeyValue] -- ^ other arguments
    -> IO ()
broadcastMType hi name msg args {- otherArgs -} = do
  let mvar = hiState hi
      sendName = fromClientName name
  hub <- readMVar mvar
  let clients = findOtherSubscribedClients (hiClients hub) name msg
      hir = hiReader hi
  forM_ clients (\cd -> notifyRecipient sendName cd msg args [])
        
  -- TODO: do I need to fix hub handling of the message or is this okay?
  --       
  when (hiName hir /= fromClientName name &&
        memberSubMap msg (hiSubscribed hir))
           (dbgIO ("**** skipping notifying hub about " ++ show msg))

           
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
    in broadcastMType hi hubName "samp.hub.event.register"
           [("id", toSValue name)]

broadcastRemovedClient ::
  HubInfo
  -> ClientName  -- ^ the client that has just been removed
  -> IO ()
broadcastRemovedClient hi name =
    let hubName = toClientName (hiName (hiReader hi))
    in broadcastMType hi hubName "samp.hub.event.unregister"
           [("id", toSValue name)]
    
broadcastNewMetadata ::
  HubInfo
  -> ClientName  -- ^ the client that has new metadata
  -> MetadataMap   -- ^ the new metadata
  -> IO ()
broadcastNewMetadata hi name mdata =
    let hubName = toClientName (hiName (hiReader hi))
        kvs = M.toList mdata
    in broadcastMType hi hubName "samp.hub.event.metadata"
           [("id", toSValue name)
           , ("metadata", SAMPMap kvs)]
                                                  
broadcastNewSubscriptions ::
  HubInfo
  -> ClientName  -- ^ the client that has new subscriptions
  -> SubscriptionMap -- ^ the new subscriptions
  -> IO ()
broadcastNewSubscriptions hi name subs =
    let hubName = toClientName (hiName (hiReader hi))
        conv (mt, xs) = (mtToRS mt, SAMPMap xs)
        kvs = map conv (fromSubMap subs)
    in broadcastMType hi hubName "samp.hub.event.subscriptions"
           [("id", toSValue name)
           , ("subscriptions", SAMPMap kvs)]

-- Any valid MType is a valid RString
mtToRS :: MType -> RString
mtToRS = fromJust . toRString . fromMType

{-
The response to a message - i.e. sending the notifications - can
be done in a separate thread. Depending on what gets sent to the
action (i.e. whether it is sent the hub state at the time of
the processing, or an MVar), there may be the chance for confusion,
but worry about that later.
-}
forkCall :: IO a -> ActionM ()
forkCall act = liftIO (void (forkIO (void act)))
                         
register ::
    HubInfo
    -> ActionM ()
register hi = do
  let hubName = hiName (hiReader hi)
  (clName, clKey) <- liftIO (addClientToHub hi)
  dbg ("Added client: " ++ show clName ++ " secret=" ++ show clKey)
  let kvs = [ ("samp.private-key", toSValue clKey)
            , ("samp.self-id", toSValue clName)
            , ("samp.hub-id", toSValue hubName) ]

  respond "register" (HC.makeResponse kvs)
  forkCall (broadcastNewClient hi clName)

           
-- | Indicate an error. Not sure how often this should be used
--   as opposed to respondError, which uses the XML-RPC fault mechanism.
errorResponse :: String -> L.ByteString
errorResponse msg = renderSAMPResponse (SAMPFault msg)
                    
-- | The client used an unknown client id.
invalidClient :: L.ByteString
invalidClient = rawError "Invalid client id"

-- | The client used an unknown secret.
--   
invalidSecret :: L.ByteString
invalidSecret = rawError "Invalid client secret"

unregister ::
    HubInfo
    -> ClientSecret
    -> ActionM ()
unregister hi secret = do
  let mvar = hiState hi
  ohub <- liftIO (takeMVar mvar)
  let oclientmap = hiClients ohub
      mclient = M.lookup secret oclientmap
      nclientmap = M.delete secret oclientmap
      nhub = ohub { hiClients = nclientmap }

  rsp <- case mclient of
           Just client -> do
             let clName = cdName client            
             dbg ("Removing client " ++ show clName)
             liftIO (putMVar mvar nhub)
             nclientmap `seq` nhub `seq` return ()
             forkCall (broadcastRemovedClient hi clName)

             unless (M.null (cdInFlight client))
                  (liftIO (putStrLn ("Non empty inFlightMap: " ++
                                     show (M.keys (cdInFlight client)))))
             
             return HC.emptyResponse
                    
           _ -> do
             dbg ("Unable to find a client with secret=" ++ show secret)
             liftIO (putMVar mvar ohub)
             return invalidSecret

  respond "unregister" rsp


setMetadata ::
    HubInfo
    -> ClientSecret
    -> MetadataMap
    -> IO L.ByteString
setMetadata hi secret mdata = do
  let mvar = hiState hi
  ohub <- takeMVar mvar
  let oclientmap = hiClients ohub
  let mclient = M.lookup secret oclientmap
  case mclient of
    Nothing -> putMVar mvar ohub
               >> dbgIO ("Unable to find a client with secret=" ++ show secret)
               >> return invalidSecret

    Just clinfo -> let nclinfo = clinfo { cdMetadata = mdata }
                       nclientmap = M.insert secret nclinfo oclientmap
                       nhub = ohub { hiClients = nclientmap }
                   in dbgIO ("Registering new metadata for client " ++
                             show (cdName clinfo))
                      >> putMVar mvar nhub
                      >> (mdata `seq` nclinfo `seq` nclientmap `seq`
                          nhub `seq` return ())
                      >> return HC.emptyResponse
                         
declareMetadata ::
    HubInfo
    -> ClientSecret
    -> [SAMPKeyValue]
    -> ActionM ()
declareMetadata hi secret kvs = do
  let mdata = M.fromList kvs

  respondToClient "set/metadata" (setMetadata hi secret mdata)

  -- this is wasteful: perhaps need to re-organize
  mcd <- liftIO (findClient hi secret)
  case mcd of
    Just cd -> forkCall (broadcastNewMetadata hi (cdName cd) mdata)
    _ -> return ()


-- | Return the client data associated with the secret.
findClient :: HubInfo -> ClientSecret -> IO (Maybe ClientData)
findClient hi secret = do
  clMap <- getClientMap hi
  return (M.lookup secret clMap)

-- | Return the callable client data associated with the secret.
findCallableClient :: HubInfo -> ClientSecret -> IO (Either String ClientData)
findCallableClient hi secret = do
  clMap <- getClientMap hi
  let msender = do
        cd <- case M.lookup secret clMap of
                Just v -> Right v
                _ -> Left ("Unable to find a client with secret=" ++
                           fromRString (fromClientSecret secret))

        when (isNothing (cdCallback cd))
             (Left ("Client " ++ show (cdName cd) ++ " is not callable"))
        Right cd

  return msender

-- | Return the clients, labelled by their secrets.
getClientMap :: HubInfo -> IO ClientMap
getClientMap hi = do
  hub <- readMVar (hiState hi)
  return (hiClients hub)

         
setCallback ::
    HubInfo
    -> ClientSecret
    -> String         -- ^ the URL   
    -> IO L.ByteString
setCallback his secret url = do
  let mvar = hiState his

      -- the only reason for IO here is the logging and trying 
      -- a strict insert
      act ohub =
        let oclientmap = hiClients ohub
            mclient = M.lookup secret oclientmap
        in case mclient of
             Nothing -> do
               dbgIO ("Unable to find a client with secret=" ++ show secret)
               -- TODO: is errorResponse correct here, or invalidClient
               return (ohub, (errorResponse "Invalid client id", Nothing))

             Just clinfo ->
                 let nclinfo = clinfo { cdCallback = Just url }
                     nclientmap = M.insert secret nclinfo oclientmap
                     nhub = ohub { hiClients = nclientmap }
                 in do
                   dbgIO ("Registering Callback for client " ++
                          show (cdName clinfo))
                   return (nhub, (HC.emptyResponse,
                                  Just (nclinfo `seq` nclientmap `seq`
                                        nhub)))
             
  (rsp, ms) <- modifyMVar mvar act
  case ms of
    Just s -> s `seq` return () -- perhaps should just use a strict MVar
    _ -> return ()
  return rsp

         
setXmlrpcCallback ::
    HubInfo
    -> ClientSecret
    -> RString        -- ^ the URL for the callback   
    -> ActionM ()
setXmlrpcCallback hi secret url =
    let urls = fromRString url
        act = case parseURI urls of
                Just _ -> liftIO (setCallback hi secret urls)
                Nothing -> return (errorResponse "Invalid url")
    in respondToClient "set/callback" act

          
setSubscriptions ::
    HubInfo
    -> ClientSecret
    -> SubscriptionMap
    -> IO L.ByteString
setSubscriptions hi secret subs = do
  let mvar = hiState hi
  ohub <- takeMVar mvar
  let oclientmap = hiClients ohub
      mclient = M.lookup secret oclientmap
  case mclient of
    Nothing -> putMVar mvar ohub
               >> dbgIO ("Unable to find a client with secret=" ++ show secret)
               >> return invalidSecret

    Just clinfo -> do
        let nclinfo = clinfo { cdSubscribed = subs }
            nclientmap = M.insert secret nclinfo oclientmap
            nhub = ohub { hiClients = nclientmap }

        dbgIO ("Registering new subscriptions for client " ++
               show (cdName clinfo))
        putMVar mvar nhub
        -- QUS: does this fully evaluate everything?
        subs `seq` nclinfo `seq` nclientmap `seq` nhub `seq` return ()
        return HC.emptyResponse

                    
declareSubscriptions ::
    HubInfo
    -> ClientSecret
    -> SubscriptionMap
    -> ActionM ()
declareSubscriptions hi secret subs = do
  respondToClient "set/subs" (setSubscriptions hi secret subs)

  -- this is wasteful: perhaps need to re-organize
  mcd <- liftIO (findClient hi secret)
  case mcd of
    Just cd -> forkCall (broadcastNewSubscriptions hi (cdName cd) subs)
    _ -> return ()


getRegisteredClients ::
    HubInfo
    -> ClientSecret
    -> IO L.ByteString
getRegisteredClients hi secret = do
  clientMap <- getClientMap hi
  case M.lookup secret clientMap of
    Nothing -> dbgIO ("Unable to find a client with secret=" ++ show secret)
               >> return invalidSecret

    Just clinfo ->
        let clients = SAMPList (map SAMPString (hiName (hiReader hi) :
                                                mapMaybe selClient (M.elems clientMap)))

            selClient cd = if cdSecret cd == secret
                           then Nothing
                           else Just (fromClientName (cdName cd))
        in do
          dbgIO ("Found registered clients other than client " ++
                 show (cdName clinfo))
          return (renderSAMPResponse (SAMPReturn clients))


getSubscribedClients ::
    HubInfo
    -> ClientSecret
    -> MType         -- ^ does not include a wildcard
    -> IO L.ByteString
getSubscribedClients hi secret msg = do
  clientMap <- getClientMap hi

  let noClient = do
        dbgIO ("Unable to find a client with secret=" ++ show secret)
        return invalidSecret

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
      -- do not need to include a name checkk here
      getHubSubs = do
        let hir = hiReader hi
        matchSubs <- procSubs (hiSubscribed hir)
        return (hiName hir, matchSubs)
        
      subs = catMaybes (getHubSubs : map getSubs (M.elems clientMap))
               
  case M.lookup secret clientMap of
    Nothing -> noClient

    Just clinfo -> do
      dbgIO ("Found subscribed clients other than client " ++
             show (cdName clinfo))
      return (HC.makeResponse subs)

          
-- is this going to be general enough?
extractData ::
    HubInfo
    -> ClientName
    -> ClientSecret
    -> (HubInfoReader -> a)
    -> (ClientData -> a)
    -> IO (Bool, [a])
extractData hi name secret hubQuery clQuery = do
  hub <- readMVar (hiState hi)
  let clientmap = hiClients hub
      hir = hiReader hi
      isClient = M.member secret clientmap
      find cd = cdName cd == name
      mquery = if hiName hir == fromClientName name
               then [hubQuery hir]
               else map clQuery (filter find (M.elems clientmap))
  return (isClient, mquery)

         
getMetadata ::
    HubInfo
    -> ClientSecret
    -> ClientName    -- ^ the client whose metadata is being requested
    -> IO L.ByteString
getMetadata hi secret clName = do
  (isClient, mquery) <- extractData hi clName secret hiMetadata cdMetadata
  if isClient
    then
      case mquery of
         (query:_) -> 
             let kvs = M.assocs query
             in do
               dbgIO ("Found a match for client: " ++ show clName)
               return (HC.makeResponse kvs)
                 
         _ -> do
           dbgIO ("Unable to find the requested client: " ++ show clName)
           return invalidClient

    else do
      dbgIO ("Unable to find a client with secret=" ++ show secret)
      return invalidSecret


getSubscriptions ::
    HubInfo
    -> ClientSecret
    -> ClientName    -- ^ the client whose subscriptions is being requested
    -> IO L.ByteString
getSubscriptions hi secret clName = do
  (isClient, mquery) <- extractData hi clName secret hiSubscribed cdSubscribed
  if isClient
    then
      case mquery of
        (query:_) ->
            let svals = fromSubMap query
                conv (k, kvs) = (mtToRS k, SAMPMap kvs)
            in do
              dbgIO ("Found a match for client: " ++ show clName)
              return (HC.makeResponse (map conv svals))
                 
        _ -> do
          dbgIO ("Unable to find the requested client: " ++ show clName)
          return invalidClient

    else do
      dbgIO ("Unable to find a client with secret=" ++ show secret)
      return invalidSecret

                   
-- experimental JSON responses

jsonGetClients ::
    HubInfo
    -> IO (M.Map String [String])
jsonGetClients hi = do
  let hir = hiReader hi
      mvar = hiState hi
  hub <- readMVar mvar
  let clMap = hiClients hub
      out = hiName hir : map (fromClientName . cdName) (M.elems clMap)
      names = map fromRString out
  return (M.fromList [("clients", names)])

-- | Return the metadata. The outer map has the key
--   "metadata". Its value is a map with the client
--   name as the key and the value is the metadata.
jsonGetMetadata ::
    HubInfo
    -> IO (M.Map String (M.Map String (M.Map String SAMPValue)))
jsonGetMetadata hi = do
  let hir = hiReader hi
      mvar = hiState hi
  hub <- readMVar mvar

  let clMap = hiClients hub

      -- Leaving the return as MetadataMap rather than Map String SAMPValue
      -- seemed ot lead to it being treated as [SAMPValue] instead - i.e.
      -- the keys wer elost (or I was doing something very stupid)
      mapKey = M.mapKeys fromRString
      conv cd = (fromClientName (cdName cd),
                 mapKey (cdMetadata cd))

      mdata = (hiName hir, mapKey (hiMetadata hir)) :
              map conv (M.elems clMap)

      converted = map (first fromRString) mdata
                  
  return (M.fromList [("metadata", M.fromList converted)])

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
jsonGetSubscriptions hi = do
  let hir = hiReader hi
      mvar = hiState hi
  hub <- readMVar mvar
  let clMap = hiClients hub

      conv (mtype, minfo) = (fromMType mtype,
                             M.fromList (map c minfo))
          where
            c (k, vs) = (fromRString k, J.toJSON vs)
      convSubs smap = M.fromList (map conv (fromSubMap smap))

      hubSubs = (hiName hir, convSubs (hiSubscribed hir))
      toSub cd = (fromClientName (cdName cd), convSubs (cdSubscribed cd))
      out = hubSubs : map toSub (M.elems clMap)
      subs = M.fromList (map (first fromRString) out)

  return (M.fromList [("subscriptions", subs)])
         
         
-- | Return the callback endpoints of those clients which have
--   called samp.hub.setXmlrpcCallback.
--
jsonGetCallbacks ::
    HubInfo
    -> IO (M.Map String (M.Map String String))
       -- ^ The returned object has the key \"callbacks\". The
       --   contents are labelled with the client name, each of
       --   which gives the URI, including the hub.
jsonGetCallbacks hi = do
  let hir = hiReader hi
      mvar = hiState hi
  hub <- readMVar mvar
  let clMap = hiClients hub

      fromName = fromRString . fromClientName
      conv cd = do
        cback <- cdCallback cd
        return (fromName (cdName cd), cback)

      hubInfo = (fromRString (hiName hir), hiCallback hir)
               
      out = hubInfo : mapMaybe conv (M.elems clMap)
      cbs = M.fromList out

  return (M.fromList [("callbacks", cbs)])
