{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2023
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------
-}

{-

Usage:

   ./viewhub

Aim:

View hub and clients. Based on Snooper.hs

-}

module Main (main) where

import qualified Brick as B
import qualified Brick.BChan as BC
import qualified Brick.Widgets.Table as WT
import qualified Brick.Widgets.List as WL
import qualified Control.Exception as CE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isUserError, ioeGetErrorString)

import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO,
                                    readTVar, readTVarIO,
                                    writeTVar)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

import Graphics.Vty.Attributes (defAttr
                               , green
                                 -- , magenta
                               , reverseVideo)
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Input.Events (Event(..), Key(..))
import Graphics.Vty.Platform.Unix (mkVty)

import Network.SAMP.Standard (RString
                             , SAMPValue(..)
                             , SAMPMapValue
                             , SAMPResponseFunc
                             -- , MessageId
                             , ClientName
                             , SAMPMessage
                             , SAMPCallMap
                             , SAMPCallFunc
                             , SAMPConnection(..)
                             , SAMPNotificationMap
                             , SAMPNotificationFunc
                             , withSAMP
                             , unregisterE
                             -- , isSAMPSuccess
                             , setXmlrpcCallbackE
                             , declareSubscriptionsSimpleE
                             , runE
                             , getRegisteredClientsE
                             , getMetadataE
                             , toSAMPResponse
                             , fromRString
                             , toRStringE
                             -- , fromMessageId
                             , fromMType
                             , toClientName
                             , fromClientName
                             , getSAMPMessageType
                             , getSAMPMessageParams
                             -- , getSAMPMessageExtra
                             -- , getSAMPResponseErrorTxt
                             , simpleClientServer)
import Network.SAMP.Standard.Server.Scotty (runServer)

import qualified Network.Socket as N
import Network.Socket (Socket)

import Utils (createClient
              , getAddress
              , getSocket)


-- Track the selected client, the mapping between internal and "nice" names,
-- and the metadata for the selected client.
--

type DisplayName = T.Text
type Metadata = (T.Text, T.Text)
type ClientMap = M.Map ClientName DisplayName
type MetadataMap = M.Map ClientName [Metadata]
type Notify = (DisplayName, T.Text, [Metadata])
  
type Storage = TVar (ClientMap, MetadataMap)

data Resource = Clients | Metadata
  deriving (Eq, Ord, Show)

-- The SAMPMessage event is for debugging, to send back a message
-- that causes the process to end and gets displayed.
--
data SAMPEvent = SAMPChange
  | SAMPShutdown
  | SAMPNotify Notify
  | SAMPMessage String


data TuiState = TuiState {
  tuiStorage :: Storage   -- we could just send this in directly
  , tuiClientList :: WL.List Resource (ClientName, DisplayName)
  , tuiMetadataList :: WT.Table Resource
  , tuiNotices :: [Notify]
  , tuiMessage :: Maybe String
  }


mkTable :: [[B.Widget n]] -> WT.Table n
mkTable = WT.surroundingBorder False .
          WT.rowBorders False .
          WT.table

          
createMetadataTable ::
  MetadataMap
  -> WL.List Resource (ClientName, DisplayName)
  -> WT.Table Resource
createMetadataTable mmap clients =
  let mlist = case WL.listSelectedElement clients of
        Just (_, (clname, _)) -> M.findWithDefault [] clname mmap
        _ -> []

      rows = map (\(k,v) -> [B.txt k, B.txt v]) mlist

  in mkTable rows

  
setupTUI :: Storage -> [Notify] -> IO TuiState
setupTUI svar notices = do
  (cmap, mmap) <- readTVarIO svar
  let clist = V.fromList . M.assocs $ cmap
      clients = WL.list Clients clist 1
      mtable = createMetadataTable mmap clients
      
  pure $ TuiState svar clients mtable notices Nothing


usage :: IO ()
usage = do
    n <- getProgName
    let estr = "Usage: " <> n
    hPutStrLn stderr estr


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> return ()
        _ -> usage >> exitFailure

    withSAMP setupEverything


setupEverything :: IO ()
setupEverything = do
    sock <- getSocket
    svar <- newTVarIO (M.empty, M.empty)
    sampChan <- BC.newBChan 5
    
    -- TODO: do I want a channel to send messages? think about laterz

    -- is this excessive?
    let closeSock = N.close sock

        hdlr :: String -> IO ()
        hdlr emsg = closeSock
                    >> hPutStrLn stderr ("ERROR: " ++ emsg)
                    >> exitFailure
         
        ioHdlr :: CE.IOException -> IO ()
        ioHdlr e = let emsg = if isUserError e
                              then ioeGetErrorString e
                              else show e
                   in hdlr emsg

        -- this assumes the only way to get a ThreadKilled is via a shutdown message
        asyncHdlr :: CE.AsyncException -> IO ()
        asyncHdlr e = let emsg = if e == CE.ThreadKilled
                                 then "SAMP Hub has--  shut down."
                                 else show e
                      in hdlr emsg

        otherHdlr :: CE.SomeException -> IO ()
        otherHdlr e = hdlr (show e)

        handlers = [ CE.Handler ioHdlr
                   , CE.Handler asyncHdlr
                   , CE.Handler otherHdlr ]


    tid <- myThreadId
    
    -- Start the SAMP client process in a separate thread. The
    -- error handling is now really-not-well designed!
    --
    sampThreadId <- forkIO (snoop tid sock svar sampChan `CE.catches` handlers)

    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    
    initialState <- setupTUI svar []

    finalState <- B.customMain initialVty buildVty
                  (Just sampChan) tuiApp initialState

    case tuiMessage finalState of
      Just msg -> hPutStrLn stderr $ "*** debug: " <> msg
      _ -> pure ()

    -- we need to send an "unregister" signal
    killThread sampThreadId
    
    closeSock
    exitSuccess


tuiApp :: B.App TuiState SAMPEvent Resource
tuiApp =
  let attrs = B.attrMap defAttr attrList
      attrList = [ (B.attrName "selected", B.style reverseVideo)
                 -- , (B.attrName "client", B.fg magenta)
                 , (B.attrName "column", B.fg green)
                 ]

  in B.App
     { B.appDraw = drawApp
     , B.appChooseCursor = B.neverShowCursor
     , B.appHandleEvent = handleEvent
     , B.appStartEvent = pure ()
     , B.appAttrMap = const attrs
     }


withAttrName :: String -> B.Widget n -> B.Widget n
withAttrName n = B.withAttr (B.attrName n)


drawApp :: TuiState -> [B.Widget Resource]
drawApp (TuiState _ clients metadata notices _) =
  let lhs = column "clients" (drawList clients)
      rhs = column "metadata" (drawTable metadata)

      top = B.hBox [B.hLimit 20 lhs, rhs]
      btm = column "notifications" (drawNotify notices)

  in [B.vBox [B.vLimit 15 top
             , btm]]


-- add a label at the top of the column
column :: T.Text -> B.Widget n -> B.Widget n
column lbl cts =
  let hdr = B.padLeftRight 1 $
            B.hBox [fillChar
                    , withAttrName "column" (B.padLeftRight 1 (B.txt lbl))
                    , fillChar]

      -- try to make this "not greedy" for height
      fillChar = B.vLimit 1 (B.fill '-')
      
  in B.vBox [hdr, cts]
                 

drawList :: (Show a, Ord a) => WL.List a (ClientName, DisplayName) -> B.Widget a
drawList list | isEmpty = B.txt "no data!"
              | otherwise = WL.renderList draw True list
  where
    isEmpty = null (WL.listElements list)
    draw selected (_, n) =
      let f = if selected then withAttrName "selected" else id
          -- g = if cl == this then withAttrName "client" else id
          g = id
      in f . g $ B.txt n


drawTable :: WT.Table n -> B.Widget n
drawTable = WT.renderTable


drawNotify :: [Notify] -> B.Widget a
drawNotify [] = B.txt "no messages"
drawNotify ns = WT.renderTable . mkTable $ tbl
  where
    tbl = map conv ns
    conv (clname, msg, md) =
      let c1 = B.txt clname
          c2 = B.txt msg
          c3 = B.txt ptxt

          params = map (\(k, v) -> k <> "=" <> v) md
          ptxt = T.intercalate ", " params
          
      in [c1, c2, c3]


handleEvent ::
  B.BrickEvent Resource SAMPEvent
  -> B.EventM Resource TuiState ()
handleEvent (B.VtyEvent (EvKey (KChar 'q') _)) = B.halt
handleEvent (B.VtyEvent (EvKey (KChar 'Q') _)) = B.halt

-- debug
handleEvent (B.AppEvent (SAMPMessage msg)) = do
  st <- B.get
  B.put $ st { tuiMessage = Just msg }
  B.halt

handleEvent (B.AppEvent SAMPShutdown) = B.halt
handleEvent (B.AppEvent SAMPChange) = do
  st <- B.get

  -- we could try to keep the old selected item (should be
  -- reasonably easy to do) but for now skip
  --
  nst <- liftIO (setupTUI (tuiStorage st) (tuiNotices st))
  B.put nst

handleEvent (B.AppEvent (SAMPNotify n)) = do
  st <- B.get
  let onotices = tuiNotices st
      notices = n : onotices
  B.put $ st { tuiNotices = notices }

    
handleEvent ev = do
  st <- B.get

  -- reading the map is a bit OTT here
  (_, mmap) <- liftIO $ readTVarIO (tuiStorage st)
  
  let olist = tuiClientList st
      handle nlist = st {
        tuiClientList = nlist
        , tuiMetadataList = createMetadataTable mmap nlist
        }

  scrollHandler handle olist ev
  
-- handleEvent _ = pure ()

scrollHandler ::
  Ord n
  => (WL.List n e -> s)
  -> WL.List n e
  -> B.BrickEvent m f
  -> B.EventM n s ()
scrollHandler restore list (B.VtyEvent ev) = do
  nlist <- B.nestEventM' list (WL.handleListEvent ev)
  B.put (restore nlist)

scrollHandler _ _ _ = pure ()  -- assume we do nothing

  
sName :: RString
sName = "samp.name"

{-
Will need a way to send messages to the TUI to say

  - close down hub
  - remove client
  - add client
  - change client metadata
  - seen a message

but to start with could just query the current state and display
them (so just list what's available).

-}



{-

We need to create a server to service messages from the Hub as well as
the SAMP client. We don't actually start the server up until after
registering for the calls, which means there is a small window when we
won't receive messages, but I am willing to live with this for now.

-}

snoop :: ThreadId -> Socket -> Storage -> BC.BChan SAMPEvent -> IO ()
snoop parentThread sock svar sampChan = do
    -- create the client
    conn <- runE (createClient "hsamp-tui"
                   "View clients and messages connected to the SAMP hub")

    -- the assumption is that we only need to unregister if we get
    -- a user interrupt but not for other errors; this doesn't seem
    -- that sensible an assumption, so remove for now
    {-
    let cleanUp :: CE.AsyncException -> IO ()
        cleanUp e = when (e == CE.UserInterrupt) (runE (unregisterE conn))
                    >> CE.throwIO e
    -}
    let cleanUp :: CE.SomeException -> IO ()
        cleanUp e = runE (unregisterE conn)
                    >> CE.throwIO e

    updateMetadataMap svar conn
    BC.writeBChan sampChan SAMPChange
    runSnooper parentThread sock svar sampChan conn
      `CE.catch` cleanUp

-- Set up the subscriptions and run the server to process the requests.

runSnooper ::
  ThreadId
  -> Socket
  -> Storage
  -> BC.BChan SAMPEvent
  -> SAMPConnection
  -> IO ()
runSnooper parentThread sock svar sampChan conn = do
    -- what is the address of the server?
    url <- getAddress sock
    urlR <- runE (toRStringE url)

    -- register the messages that we are interested in
    runE (setXmlrpcCallbackE conn urlR >>
          declareSubscriptionsSimpleE conn ["*"])

    -- now run the server
    runServer sock url (processCall conn parentThread svar sampChan)

toText :: RString -> T.Text
toText = T.pack . fromRString

bracket :: RString -> T.Text
bracket a = " (" <> toText a <> ")"

toName :: ClientName -> Maybe RString -> T.Text
toName k mv =
  let prefix = toText (fromClientName k)
  in prefix <> maybe "" bracket mv


-- assume we only want metadata that we can convert to a string

convertMetadata :: SAMPMapValue -> [Metadata]
convertMetadata md =
  let conv (k, v) = (toText k,
                     case v of
                       SAMPString s -> toText s
                       SAMPList _ -> "<list>"
                       SAMPMap _ -> "<map>"
                    )

  in map conv (M.toList md)


{-
This removes the existing data, which is "okay" as it's
only expected to be used when the input maps are empty. It's not a
great scheme!

There is likely a chance that we could lose information on a new
or deleted client.
-}
updateMetadataMap :: Storage -> SAMPConnection -> IO ()
updateMetadataMap svar conn = do
  
  let get = do
        clients <- getRegisteredClientsE conn

        -- need to add on this client to the list
        let aclients = scId conn : clients
        
        mds <- forM aclients $ \cl -> do
          md <- getMetadataE conn cl

          let name = case M.lookup sName md of
                Just (SAMPString s) -> toName cl (Just s)
                _ -> toName cl Nothing
              
              mdList = convertMetadata md

          pure (cl, name, mdList)

        let clinfo = map (\(a, b, _) -> (a, b)) mds
            mdinfo = map (\(a, _, c) -> (a, c)) mds
            
        pure (M.fromList clinfo, M.fromList mdinfo)

      replace nmap = modifyTVar' svar (const nmap)
      
  clients <- runE get
  atomically (replace clients)


processCall ::
    SAMPConnection
    -> ThreadId
    -> Storage
    -> BC.BChan SAMPEvent
    -> String
    -> IO ()
processCall conn tid svar sampChan = 
    simpleClientServer conn
       (notifications tid svar sampChan)
       (calls sampChan)
       rfunc

{-

TODO: should we have a handler for samp.hub.event.* which then 

  - checks for id field
  - processes remaining contents

or at least abstract out this for the messages we do support.

-}

notifications ::
  ThreadId -> Storage -> BC.BChan SAMPEvent -> SAMPNotificationMap
notifications tid svar sampChan =
     [("samp.hub.event.register", handleRegister svar sampChan)
     , ("samp.hub.event.unregister", handleUnregister svar sampChan)
     , ("samp.hub.event.metadata", handleMetadata svar sampChan)
       -- for now we don't track the individual subscriptions
       -- , ("samp.hub.event.subscriptions", handleSubscriptions)
     , ("samp.hub.event.shutdown", handleShutdown sampChan tid)
     , ("*", handleOther sampChan)
     ]

calls :: BC.BChan SAMPEvent -> SAMPCallMap
calls sampChan =
    [("samp.app.ping", handlePingCall sampChan)
     -- , ("*", handleOtherCall)
    ]


sendNotify ::
  BC.BChan SAMPEvent
  -> ClientName
  -> SAMPMessage
  -> IO ()
sendNotify sampChan name msg = 
  let mtype = T.pack . fromMType $ getSAMPMessageType msg
      mdata = convertMetadata (getSAMPMessageParams msg)
      clname = toText $ fromClientName name
      
  in BC.writeBChan sampChan (SAMPNotify (clname, mtype, mdata))


-- Now we have sampChan we should not need the killThread (or, perhaps
-- we need to change it back to killing the correct thread).
--
handleShutdown :: BC.BChan SAMPEvent -> ThreadId -> SAMPNotificationFunc
handleShutdown sampChan tid _ name msg = do
  sendNotify sampChan name msg
  BC.writeBChan sampChan SAMPShutdown
  killThread tid


getText :: RString -> SAMPMapValue -> Maybe RString
getText k v = case M.lookup k v of
  Just (SAMPString s) -> Just s
  _ -> Nothing
  

-- Registering just sets up entries in the two maps. They will hopefully be
-- over-written once the metadata has been updated.
--
-- What is the name argument? Could we drop the "getText "id" params
-- but?
handleRegister ::
    Storage
    -> BC.BChan SAMPEvent
    -> SAMPNotificationFunc
handleRegister svar sampChan _ name msg = do
  sendNotify sampChan name msg

  let params = getSAMPMessageParams msg

      update clid = do
        (cmap, mmap) <- readTVar svar
        let clname = toClientName clid

            cmap2 = M.insert clname (toName clname Nothing) cmap
            mmap2 = M.insert clname [] mmap
            
        writeTVar svar (cmap2, mmap2)
          
  case getText "id" params of
    Just clid -> atomically (update clid) >>
                 BC.writeBChan sampChan SAMPChange
    _ -> pure ()


handleUnregister ::
    Storage
    -> BC.BChan SAMPEvent
    -> SAMPNotificationFunc
handleUnregister svar sampChan _ name msg = do
  sendNotify sampChan name msg

  let params = getSAMPMessageParams msg

      update clid = do
        (cmap, mmap) <- readTVar svar
        let clname = toClientName clid
        
            cmap2 = M.delete clname cmap
            mmap2 = M.delete clname mmap
            
        writeTVar svar (cmap2, mmap2)
          
  case getText "id" params of
    Just clid -> atomically (update clid) >>
                 BC.writeBChan sampChan SAMPChange
    _ -> pure ()
      

handleMetadata ::
    Storage
    -> BC.BChan SAMPEvent
    -> SAMPNotificationFunc
handleMetadata svar sampChan _ name msg = do
  sendNotify sampChan name msg

  let params = getSAMPMessageParams msg
      mdMap = case M.lookup "metadata" params of
        Just (SAMPMap ms) -> ms
        _ -> M.empty

      update clid mname = do
        (cmap, mmap) <- readTVar svar
        let clname = toClientName clid

            md = convertMetadata mdMap
            
            cmap2 = M.insert clname (toName clname mname) cmap
            mmap2 = M.insert clname md mmap
            
        writeTVar svar (cmap2, mmap2)
          
  case getText "id" params of
    Just clid -> let mname = getText sName mdMap
                 in atomically (update clid mname) >>
                    BC.writeBChan sampChan SAMPChange
                    
    _ -> pure ()


handleOther :: BC.BChan SAMPEvent -> SAMPNotificationFunc
handleOther sampChan _ name msg = sendNotify sampChan name msg


handlePingCall :: BC.BChan SAMPEvent -> SAMPCallFunc
handlePingCall sampChan _ name _ msg = do
  sendNotify sampChan name msg
  pure (toSAMPResponse M.empty M.empty)


-- TODO: handle warning case, although when is this ever called?

rfunc :: SAMPResponseFunc
rfunc _ _ _ _ = pure ()

