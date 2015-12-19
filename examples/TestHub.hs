{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------
-}

{-
Test out the SAMP hub.

It is based on the tests run by the JSAMP HubTester application.

From http://www.star.bris.ac.uk/~mbt/jsamp/

  JSAMP is open source software; it is available under (at least) the
  Academic Free Licence and the BSD Licence.

Usage:

  ./testhub --debug

-}

module Main (main) where

import qualified Data.Set as Set
import qualified Data.Map as M

import Control.Arrow (first)

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)

import Control.Monad (forM_, unless, when, void)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, catchError, throwError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random (MonadRandom(..), Rand, runRand, uniform)
import Control.Monad.Trans.State.Strict ( StateT
                                        -- , evalStateT
                                        , execStateT
                                        , get, modify', put)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)

import Data.Char (chr)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (partition, sort)
import Data.Maybe (fromJust, isJust)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- import Data.Version (showVersion)

import Network.SAMP.Standard ( SAMPInfo, SAMPConnection(..)
                             , Err
                             , handleError
                             , getSAMPInfoHubURL, getSAMPInfoHubMetadata
                             , registerClientE, toMetadata
                             , toClientName
                             , runE
                             , withSAMP)
import Network.SAMP.Standard.Client ( callAndWaitE
                                    , declareMetadataE
                                    , declareSubscriptionsE
                                    , declareSubscriptionsSimpleE
                                    , getRegisteredClientsE
                                    , getMetadataE
                                    , getSubscriptionsE
                                    , getSubscribedClientsE
                                    , notifyAllE
                                    , notifyE
                                    , pingHubE
                                    , unregisterE)
import Network.SAMP.Standard.Setup (sampLockFileE, getHubInfoE)
import Network.SAMP.Standard.Server ( SAMPNotificationMap
                                    , SAMPNotificationFunc
                                    , SAMPCallMap
                                    , SAMPCallFunc
                                    , SAMPResponseFunc
                                    , callAllE
                                    , callE
                                    , setXmlrpcCallbackE
                                    , simpleClientServer)
import Network.SAMP.Standard.Server.Scotty (runServerSignal)
import Network.SAMP.Standard.Types ( ClientName
                                   , ClientSecret
                                   , MessageId
                                   , MessageTag
                                   , MType
                                   , RString
                                   , SAMPMapValue
                                   , SAMPMessage
                                   , SAMPResponse
                                   , SAMPType
                                   , SAMPValue(..)
                                   , dumpSAMPValue
                                   , fromMType
                                   , getKey
                                   , getSAMPMessageExtra
                                   , getSAMPMessageType
                                   , getSAMPMessageParams
                                   , getSAMPResponseError
                                   , getSAMPResponseExtra
                                   , getSAMPResponseResult
                                   , isSAMPError
                                   , isSAMPErrorOnly
                                   , isSAMPSuccess
                                   , removeKeyE
                                   , toMessageTag
                                   , toRString
                                   , toRStringE
                                   , toSAMPMessageMT
                                   , toSAMPResponse
                                   , toSAMPResponseError
                                   , toSValue
                                   )

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Log.Logger
import System.Random (RandomGen, newStdGen)

-- import Paths_hsamp (version)

import Utils (getAddress, getSocket)

data HubStore =
  HubStore
  {
    _hsOtherClients :: [ClientName]
    -- ^ Clients that are known to exist (or existed) that are not
    --   related to the hub. These will be ignored in tests and checks.
    --   Ideally this would be in a reader monad, since the list isn't
    --   expected to change once created.
  , _hsUsedNames :: [ClientName]
    -- ^ names used by the hub (kept so that we can check that names are
    --   not being re-used)
  , _hsUsedSecrets :: [ClientSecret]
    -- ^ secrets used by the hub (kept so that we can check that names are
    --   not being re-used)
  }


emptyHubStore :: HubStore
emptyHubStore = 
  HubStore
  { 
    _hsOtherClients = []
  , _hsUsedNames = []
  , _hsUsedSecrets = []
  }


-- | ping messages increase a global counter
type PingCount = IORef Int

newPingCount :: IO PingCount
newPingCount = newIORef 0

increasePingCount :: PingCount -> IO ()
increasePingCount pc =
  let incr v = (succ v, ())
  in atomicModifyIORef' pc incr
  
getPingCount :: PingCount -> IO Int
getPingCount = readIORef

-- | Store the responses to a client, labelled by the message
--   tag.
--
type ResponseMap = IORef [((ClientName, MessageTag), SAMPResponse)]

newResponseMap :: IO ResponseMap
newResponseMap = newIORef []

countResponse :: ResponseMap -> IO Int
countResponse rmap = length `fmap` readIORef rmap

-- | Add the response to the start of the list.
--
addResponse ::
  ResponseMap -> ClientName -> MessageTag -> SAMPResponse -> IO ()
addResponse rmap clId msgTag rsp =
  let newVal = ((clId, msgTag), rsp)
      add kvs = (newVal : kvs, ())
  in atomicModifyIORef' rmap add

-- | Removes all entries that match this client and tag. The return value
--   can be empty.
--
--   Subject to change. Is this atomic? Use STM instead?
removeResponse ::
  ResponseMap -> ClientName -> MessageTag -> IO [SAMPResponse]
removeResponse rmap clId msgTag =
  let match (k,_) = k == (clId, msgTag)
      getV kvs = let (as, bs) = partition match kvs
                 in (bs, map snd as)
  in atomicModifyIORef' rmap getV


-- | Wait until the given response has been returned. It
--   returns a single response, even if there are multiple
--   ones in the store.
--
--   This is a simple polling loop and there is no time out.
--
waitForResponse ::
  ResponseMap -> ClientName -> MessageTag -> IO SAMPResponse
waitForResponse rmap clId msgTag = do
  let key = (clId, msgTag)
      
      -- I wanted to just read the map in this loop, but there's
      -- a potential race if the extraction step is separate from
      -- the check step.
      --
      -- Only interested in the first key in this case
      getKV kvs = case break ((== key) . fst) kvs of
        (_, []) -> (kvs, Nothing)
        (l, (h:r)) -> (l ++ r, Just (snd h))
          
      go = do
        mrsp <- atomicModifyIORef' rmap getKV
        case mrsp of
          Just rsp -> return rsp
          _ -> waitMillis 100 >> go
          
  go

-- transformer stack: layer State on top of Err
--
type HubTest m a = StateT HubStore (ExceptT String m) a


assertTrue :: MonadError a m => a -> Bool -> m ()
assertTrue lbl f = unless f (throwError lbl)

assert ::
  (MonadError String m, Eq a, Show a) => String -> a -> a -> m ()
assert lbl expVal gotVal =
  let lbl2 = lbl ++ ":\nexpected=" ++ show expVal ++
             "\ngot     =" ++ show gotVal
  in assertTrue lbl2 (expVal == gotVal)

-- An unordered comparison of the two lists
assertSet ::
  (MonadError String m, Ord a, Eq a, Show a) => String -> [a] -> [a] -> m ()
assertSet lbl expVal gotVal =
  assert lbl (Set.fromList expVal) (Set.fromList gotVal)


assertSAMPMap ::
  (MonadError String m)
  => String -> SAMPMapValue -> SAMPMapValue -> m ()
assertSAMPMap lbl m1 m2 =
  let v1 = SAMPMap m1
      v2 = SAMPMap m2
      lbl2 = lbl ++
             ":\nexpected=\n" ++ dumpSAMPValue v1 ++
             ":\ngot     =\n" ++ dumpSAMPValue v2
  in assertTrue lbl2 (v1 == v2)
  
-- Assert that the key exists, and return that value.
assertKey ::
  (MonadError String m, Ord b, Show b)
  => String -> b -> M.Map b c -> m c
assertKey lbl k ms =
  let kstr = show k
  in case M.lookup k ms of
      Just v -> return v
      Nothing -> throwError (lbl ++ ": key not found: " ++ kstr)

      
-- Error if a is a member of the list
assertNoKey ::
  (MonadError String m, Eq a, Show a)
  => String -> a -> [(a, b)] -> m ()
assertNoKey lbl k kvs =
  unless (null (filter ((== k) . fst) kvs))
    (throwError (lbl ++ ": key should not be found: " ++ show k))
  

-- Check the list of known clients matches the input list
--   - the current connection is not included
--   - any clients that were registered when the test was
--     started are ignored
--
assertTestClients :: SAMPConnection -> [ClientName] -> HubTest IO ()
assertTestClients conn expNames = do
  state <- get
  gotNames <- lift (getRegisteredClientsE conn)
  let expSet = Set.fromList expNames

      -- remove the other clients from the list of clients,
      -- but it is possible that they have since gone away,
      -- so do not require that they exist in gotNames
      otherSet = Set.fromList (_hsOtherClients state)
      gotSet = Set.fromList gotNames `Set.difference` otherSet

  assert "Registered test clients match" expSet gotSet


-- | Check that the ping count matches the expected value
assertPing :: String -> Int -> PingCount -> HubTest IO ()
assertPing lbl count pc = do
  got <- liftIO (getPingCount pc)
  assert ("Ping count: " ++ lbl) count got


-- Overwrite the list of client names
addOtherClients :: Monad m => [ClientName] -> HubTest m ()
addOtherClients names = 
  modify' (\s -> s { _hsOtherClients = names })


-- A new client has been created, so check it doesn't clash
-- (name or secret) with previous clients and add it to the
-- store.
addClient :: Monad m => SAMPConnection -> HubTest m ()
addClient cl = do
  let clId = scId cl
      clSecret = scPrivateKey cl
  ostate <- get
  let onames = _hsUsedNames ostate
      osecrets = _hsUsedSecrets ostate

      nstate = ostate {
        _hsUsedNames = clId : onames
        , _hsUsedSecrets = clSecret : osecrets
        }

  put nstate

  assertTrue "New client id is not re-used" (clId `notElem` onames)
  assertTrue "New client secret is not re-used" (clSecret `notElem` osecrets)
  
  

checkFail :: String -> Err IO () -> HubTest IO ()
checkFail lbl call = do
  flag <- lift ((call >> return False)
                `catchError` (const (return True)))
  assertTrue ("Expected fail: " ++ lbl) flag


echoMTYPE, pingMTYPE, failMTYPE :: MType
echoMTYPE = "test.echo"
pingMTYPE = "samp.app.ping"
failMTYPE = "test.fail"

{-
-- are these used

registerMTYPE, unregisterMTYPE :: MType
registerMTYPE = "samp.hub.event.register"
unregisterMTYPE = "samp.hub.event.unregister"

metadataMTYPE, subscriptionsMTYPE :: MType
metadataMTYPE = "samp.hub.event.metadata"
subscriptionsMTYPE = "samp.hub.event.subscriptions"
-}

waitMillisKey, msgIdQueryKey, errorKey :: RString
waitMillisKey = "test.wait"
msgIdQueryKey = "test.msgid"
errorKey = "test.error"


defaultSubs :: [(MType, SAMPValue)]
defaultSubs = map (\k -> (k, emptyMap)) [echoMTYPE, pingMTYPE, failMTYPE]

-- Every legal character except for 0x0d, since there are issues when
-- transmitting this via XML (taken from Mark's comments in HubTester).
--
legalSAMPChars :: String
legalSAMPChars = map chr (0x09 : 0x0a : [0x20 .. 0x7f])

-- "printable" characters
alphaSAMPChars :: String
alphaSAMPChars = ['A' .. 'Z'] ++ ['0' .. '9']


-- | Create a "random" SAMP value. This is based on the HubTester
--   version.
--
randomSAMPValue ::
  RandomGen g
  => Int
  -- ^ maxium level of nesting; assumed to be 0 or positive
  -> Bool
  -- ^ if False then the level can be pretty printed
  -> Rand g SAMPValue
randomSAMPValue level anyPrint
  | level <= 0 = randomSAMPString anyPrint
  | otherwise  = do
      bool <- getRandom
      if bool
        then randomSAMPHashMap level anyPrint
        else randomSAMPList level anyPrint


randomSAMPString ::
  RandomGen g
  => Bool
  -- ^ if False then the level can be pretty printed
  -> Rand g SAMPValue
randomSAMPString anyPrint = do
  let maxCount = if anyPrint then 98 else 3
      fromChars = if anyPrint then legalSAMPChars else alphaSAMPChars
      
  nchar <- getRandomR (0, maxCount)
  chars <- sequence (replicate nchar (uniform fromChars))
  -- if the conversion to RString fails then there's an error in the
  -- code: treat as a run-time error
  case toEitherE (toRStringE chars) of
    Right rstr -> return (toSValue rstr)
    Left emsg -> error ("Internal error: randomSAMPString: " ++ emsg)
        

randomSAMPList ::
  RandomGen g
  => Int
  -- ^ level
  -> Bool
  -- ^ if False then the level can be pretty printed
  -> Rand g SAMPValue
randomSAMPList level anyPrint = do
  let nmax = if anyPrint then 23 else 3
      pLevel = pred level
  nel <- getRandomR (0, nmax)

  els <- sequence (replicate nel (randomSAMPValue pLevel anyPrint))
  return (SAMPList els)


randomSAMPHashMap ::
  RandomGen g
  => Int
  -- ^ level
  -> Bool
  -- ^ if False then the level can be pretty printed
  -> Rand g SAMPValue
randomSAMPHashMap level anyPrint = do
  let nmax = if anyPrint then 23 else 3
      pLevel = pred level
  nel <- getRandomR (0, nmax)
  
  -- want keys to be RString, not SAMPValue, so rely on the
  -- assumption that randomSAMPString only produces SAMPString
  let convKey (SAMPString s) = s
      convKey x = error ("Internal error: randomSAMPString: " ++ show x)
      
  keys <- sequence (replicate nel (randomSAMPString anyPrint))
  els <- sequence (replicate nel (randomSAMPValue pLevel anyPrint))
  return (SAMPMap (M.fromList (zip (map convKey keys) els)))


toMaybeE :: Err Maybe a -> Maybe a
toMaybeE = handleError (const Nothing)

toEitherE :: Err (Either String) a -> Either String a
toEitherE = handleError Left

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    ["--debug"] -> updateGlobalLogger "SAMP" (setLevel DEBUG)
    _ -> do
      pName <- getProgName
      hPutStrLn stderr ("Usage: " ++ pName ++ " [--debug]")
      exitFailure

  withSAMP (runHubTests runTests)
  exitSuccess


-- could allow in Err IO ()?
putLn :: MonadIO m => String -> m () -- String -> HubTest IO ()
putLn = liftIO . putStrLn

runHubTests :: HubTest IO a -> IO ()
-- runHubTests act = void (runE (evalStateT act emptyHubStore))
runHubTests act = do
  newState <- runE (execStateT act emptyHubStore)
  putStrLn "Final state:"
  putStrLn ("  other clients: " ++ show (_hsOtherClients newState))
  putStrLn ("  used names   : " ++ show (_hsUsedNames newState))
  putStrLn ("  used secrets : " ++ show (_hsUsedSecrets newState))

  
-- TODO:
--    hub tester has a register method that checks that the
--    hub id remains constant, that client ids are not reused,
--    and neither are private keys. So need to carry around
--    some state.
--    We shold now be doing this EXCEPT for the check on the hub
--    name
--
makeClient :: SAMPInfo -> HubTest IO SAMPConnection
makeClient si = do
  conn <- lift (registerClientE si)

  let clientName = scId conn
      clientSecret = scPrivateKey conn
  putLn ("Created client: " ++ show clientName ++ " secret: " ++
         show clientSecret)
  
  metadata <- lift (getMetadataE conn clientName)
  assertTrue "New client has no metadata" (M.null metadata)

  subscriptions <- lift (getSubscriptionsE conn clientName)
  assert "New client has no subscriptions" [] subscriptions
    
  -- verify that the client is not included in the registered
  -- clients list
  rclients <- lift (getRegisteredClientsE conn)
  assertTrue
    "New client does not appear in the getRegisteredClients call"
    (clientName `notElem` rclients)

  addClient conn
  return conn


reportHub :: SAMPInfo -> IO ()
reportHub si = do
  let prefix = "   : "
  putStrLn (prefix ++ getSAMPInfoHubURL si)
  let keys = M.toList (getSAMPInfoHubMetadata si)
      out = map (\(k,v) -> prefix ++ k ++ " => " ++ v) keys
  forM_ out putStrLn


emptyMap :: SAMPValue
emptyMap = SAMPMap M.empty

-- In HubTester there are explicit checks that the metadata
-- and subscription responses are valid. This is enforced
-- by the type checker. So this is really just a check that
-- the routines work.
--
validateClient :: SAMPConnection -> ClientName -> Err IO ()
validateClient client clientName = do
  void (getMetadataE client clientName)
  void (getSubscriptionsE client clientName)


runTests :: HubTest IO ()
runTests = do
  hubUrl <- lift sampLockFileE
  putLn ("Hub location: " ++ show hubUrl)
  hi <- lift (getHubInfoE hubUrl)
  liftIO (reportHub hi)
  
  withClient (makeClient hi) basicClient
  testClients hi
  testStress hi
  
  putLn "Finished."

{-
profile-specific tests:

  testStandardLockFile ?
  testLockInfo
     - check than can call samp.hub.ping and that
       calls fail: e.g. samp.hub.register with invalid secret and
       samp.hub.not-a-method with the secret

testClients
testStress
-}


-- Try to ensure that the client is unregistered, even if there
-- is an error. It is unlikely to catch all cases (in particular
-- asynchronous ones e.g. user interrupt).
--
-- It appears to wrap the error in another "user error" wrapper
--
-- This should not be used with callable clients (or, rather,
-- it will not kill the thread associated with such clients).
--
withClient ::
  HubTest IO SAMPConnection
  -- ^ Create the client
  -> (SAMPConnection -> HubTest IO ())
  -- ^ actions to perform with the client;
  -> HubTest IO ()
  -- ^ the client will be unregistered; there is no need to do
  --   so in the action
withClient make act = do
  cl <- make
  act cl `catchError` (\e -> lift (unregisterE cl) >> throwError e)
  lift (unregisterE cl)
  

-- This is based on code in the initializer for the
-- HubTester class.
--
basicClient :: SAMPConnection -> HubTest IO ()
basicClient client = do
  let clientName = scId client
      hubName = toClientName (scHubId client)
      hubUrl = scHubURL client
      
  putLn ("Hub name: " ++ show hubName)

  lift (pingHubE hubUrl)
  putLn "Tests need to be written!"

  {-
  -- from HubTester

  -- set up a monitor to check the hub events

  -- get hub info and check it, whatever that means
  -- including the hub id
  -}

  rclients <- lift (getRegisteredClientsE client)
  assertTrue
    "Registered clients include the hub"
    (hubName `elem` rclients)

  -- TODO:
  --   hub name is not stored
  --   store names and keys so that we can check not being reused
  addOtherClients rclients
  
  let otherClients = clientName : rclients
  forM_ otherClients (lift . validateClient client)
  putLn "Tested basicClient"


testClient0 :: SAMPConnection -> HubTest IO ()
testClient0 cl0 = do
  let name0 = "Shorty"
      desc0 = "Short-lived test client" 
      mds0 = toMetadata name0 (Just desc0) Nothing Nothing Nothing

      clId0 = scId cl0
      -- clName0 = toClientName name0
      
  lift (declareMetadataE cl0 mds0)

  -- check that the metadata contains the values just set
  mdata <- lift (getMetadataE cl0 clId0)
  mval1 <- assertKey "cl0 metadata" "samp.name" mdata
  assert "cl0 app name" (toSValue name0) mval1
  mval2 <- assertKey "cl0 metadata" "samp.description.text" mdata
  assert "cl0 description" (toSValue desc0) mval2
  
  let subCall = declareSubscriptionsSimpleE cl0 [echoMTYPE]

  -- As the client is not callable, this should error out
  checkFail "No subscription as client is not callable" subCall

  -- provide a "garbage" url (really should make sure this is not a valid
  -- end point) since there is no test of this connection (at least, no
  -- obvious test) in HubTester.java
  --
  -- TODO: set up a callable client and then kill it
  lift (setXmlrpcCallbackE cl0 "http://127.0.0.1:1234/xmlrpc")
  lift subCall

  -- check that cl0 is not listed as being registered
  subs <- lift (getSubscribedClientsE cl0 echoMTYPE)
  assertNoKey "cl0 echo MTYPE" clId0 subs
  
  putLn "Tested client 0"


defMetadata :: SAMPMapValue
defMetadata =
  let name = "Test1"
      desc = "HubTester client application" 
  in toMetadata name (Just desc) Nothing Nothing Nothing
  
-- returns the metadata  
setupClient1 :: SAMPConnection -> Err IO SAMPMapValue
setupClient1 cl1 = do
  let mds = M.insert "test.drink" "cider" defMetadata
  declareMetadataE cl1 mds
  return mds


-- returns the metadata  
setupClient2 :: SAMPConnection -> Err IO SAMPMapValue
setupClient2 cl2 = do
  let mds = M.insert "test.drink" "ribena" defMetadata
  declareMetadataE cl2 mds
  return mds
  

-- | Spawn a thread to handle the communication for this
--   callable client.
--
--   The client is marked as callable.
--
--   There is a rather basic attempt at waiting until the server
--   is ready. It is not perfect and may have ot be abandoned.
--
--   TODO: send in parent thread id so that can indicate an
--         error by sending it a ThreadKilled exception
--         (or something similar)
--
callableClient ::
  SAMPConnection
  -> IO (PingCount, ResponseMap, ThreadId)
callableClient conn = do
  mvar <- newEmptyMVar
  pingCount <- newPingCount
  responseMap <- newResponseMap

  let act = do
        sock <- getSocket
        fullUrl <- getAddress sock
        let baseUrl = reverse (dropWhile (/='/') (reverse fullUrl))
              
        -- if the URL conversion fails it will error out (via fail)
        runE (toRStringE fullUrl
              >>= setXmlrpcCallbackE conn)

        let server = simpleClientServer conn
                     (notifications pingCount)
                     (calls pingCount)
                     (rfunc responseMap)
        runServerSignal sock baseUrl server mvar

  tid <- forkIO act

  -- wait for the server to be ready (or close to ready)
  takeMVar mvar

  return (pingCount, responseMap, tid)


-- | Remove a callable client from the hub and kill the thread
--   handling the client's SAMP connection.
--
--   Note that there is no type safety here, in that nothing
--   tells the compiler that the client has been closed down.
--
--   Would like to check that the client has actually stopped
--   processing requests, but it's not clear how to do this,
--   as need a client to talk to the hub.
--
--   There is a delay before returning, to give the client a
--   chance to be stopped. This probably isn't needed.
--
removeClient :: SAMPConnection -> ThreadId -> HubTest IO ()
removeClient cl tid = do
  putLn ("Removing client: " ++ show (scId cl))
  lift (unregisterE cl)
  liftIO (killThread tid)
  waitForProcessing 100
  
-- Routines to handle SAMP conversations from a hub to a client

-- samp.client.receiveNotification
notifications ::
  PingCount
  -> SAMPNotificationMap
notifications pingCount =
  [ (echoMTYPE, ignoreEcho)
  , (pingMTYPE, notifyPing pingCount)
  , ("*", unexpectedNotification)
  ]


ignoreEcho :: SAMPNotificationFunc
ignoreEcho _ _ _ = return ()

notifyPing :: PingCount -> SAMPNotificationFunc
notifyPing pc _ clName _ = do
  putStrLn ("SENT A PING (notify from " ++ show clName ++ ")") -- DBG
  increasePingCount pc

dumpKV :: (Show a, Show b) => (a, b) -> IO ()
dumpKV (k,v) = putStrLn ("    key=" ++ show k ++ " value=" ++ show v)

unexpectedNotification :: SAMPNotificationFunc
unexpectedNotification _ clName msg = do
  let mtype = getSAMPMessageType msg
      keys = M.toList (getSAMPMessageParams msg)
      extra = M.toList (getSAMPMessageExtra msg)

  putStrLn ("Error: client " ++ show clName ++ 
            " receiveNotification=" ++ show mtype)
  putStrLn "  keys:"
  forM_ keys dumpKV
  unless (null extra) (putStrLn "  extra:" >> forM_ extra dumpKV)
  putStrLn "<-- end unexpectedNotification -->"
  error "SHOULD NOT HAVE HAPPENED"
  

-- samp.client.receiveCall
calls :: PingCount -> SAMPCallMap
calls pingCount =
  [ (echoMTYPE, callEcho)
  , (pingMTYPE, (callPing pingCount))
  , (failMTYPE, callFail)
  , ("*", unexpectedCall) -- error out
  ]


findKey :: (SAMPType a) => RString -> SAMPMapValue -> Maybe a
findKey k kvs = toMaybeE (getKey k kvs)
  
-- Delay processing if the extra params contains a waitMillisKey entry
-- that is positive.
delayCall :: SAMPMessage -> IO ()
delayCall msg = 
  let extra = getSAMPMessageExtra msg
        
  in case findKey waitMillisKey extra of
    Just waitVal | waitVal > 0 -> putStrLn ("Delaying response by (ms): " ++ show waitVal) -- DBG
                                  >> waitMillis waitVal
    _ -> return ()


-- Extract the message id if requested
addMessageId :: MessageId -> SAMPMessage -> SAMPMapValue
addMessageId msgId msg =
  case findKey msgIdQueryKey (getSAMPMessageExtra msg) of
    Just True -> M.singleton msgIdQueryKey (toSValue msgId)
    _ -> M.empty


-- SAMPCallFunc is
--   ClientSecret -> ClientName -> MessageId -> SAMPMessage -> IO SAMPResponse
--
callEcho :: SAMPCallFunc
callEcho _ _ msgId msg = do
  delayCall msg
  let params = getSAMPMessageParams msg
      other = addMessageId msgId msg
  return (toSAMPResponse params other)


callPing :: PingCount -> SAMPCallFunc
callPing pc _ _ msgId msg = do
  delayCall msg
  increasePingCount pc
  let other = addMessageId msgId msg
  return (toSAMPResponse M.empty other)

-- Can we kill the parent thread on error?
--
-- The processing code is rather ugly here, just to extract the
-- error keys from the input. Is it correct?
--
callFail :: SAMPCallFunc
callFail _ _ msgId msg = do
  delayCall msg
  let params = getSAMPMessageParams msg
      other = addMessageId msgId msg

  case toEitherE (getKey errorKey params) of
    Right (SAMPMap errs) ->
      let act = removeKeyE "samp.errortxt" errs
      in case toMaybeE act of
           Just (SAMPString emsg, e2) -> return (toSAMPResponseError emsg e2 other)
           Just (sval, _) ->
             let eval = "Expected a string for samp.errortxt, sent " ++
                        show sval
             in case toRString eval of
               Just emsg -> return (toSAMPResponseError emsg errs other)
               _ -> return (toSAMPResponseError "Internal error converting to RString" errs other)
           Nothing -> return (toSAMPResponseError "Missing samp.errortxt" errs other)
           
    Right xs -> error ("key=" ++ show errorKey ++ " was not a map but " ++
                       show xs)
    Left _ -> error ("No key=" ++ show errorKey ++ " in message")
  
-- maybe want to return an error message
unexpectedCall :: SAMPCallFunc
unexpectedCall _ clName msgId msg = do
  let mtype = getSAMPMessageType msg
      keys = M.toList (getSAMPMessageParams msg)
      extra = M.toList (getSAMPMessageExtra msg)

      other = addMessageId msgId msg

  putStrLn ("Error: client " ++ show clName ++ 
            " receiveCall=" ++ show mtype)
  putStrLn (" msg id: " ++ show msgId)
  putStrLn "  keys:"
  forM_ keys dumpKV
  unless (null extra) (putStrLn "  extra:" >> forM_ extra dumpKV)
  putStrLn "<-- end unexpectedCall -->"

  delayCall msg
  return (toSAMPResponseError
          "Client is not subscibed to this MType)" M.empty other)


-- samp.client.receiveResponse
--
-- TODO: use a mvar or channel?
rfunc ::
  ResponseMap
  -> SAMPResponseFunc
{-
rfunc rmap secret clid msgTag rsp = do
  putStrLn ("samp.client.receiveResponse called: id=" ++ show clid ++
            " secret=" ++ show secret ++ " msg-tag=" ++ show msgTag ++
            "\nresponse=" ++ show rsp) -- DBG
  addResponse rmap msgTag rsp
-}    
rfunc rmap _ clId msgTag rsp = addResponse rmap clId msgTag rsp


-- Pause processing for a small amount of time to try and let
-- threads process everything.
--
waitForProcessing ::
  MonadIO m
  => Int  -- ^ delay in milliseconds (as that's what HubTester uses)
  -> m ()
waitForProcessing = liftIO . waitMillis

waitMillis ::
  Int  -- ^ delay in milliseconds
  -> IO ()
waitMillis n = (threadDelay (n * 1000))

-- | Since the standard does not require that a timeout leads to
--   a failed connection, only warn if the call does not time out.
--
validateTimeOut :: SAMPConnection -> ClientName -> IO ()
validateTimeOut cl clId = do
  let msg = toSAMPMessageMT echoMTYPE params extra
      params = M.singleton "text" "copy"
      delay = 10000 :: Int
      extra = M.singleton waitMillisKey (toSValue delay)

  startTime <- getCurrentTime
  flag <- runExceptT (callAndWaitE cl clId msg (Just 1)) -- one second delay
  case flag of
    Right _ -> do
      endTime <- getCurrentTime
      let delta = diffUTCTime endTime startTime
          millis = delta
          delayDiff = realToFrac (fromIntegral delay / 1000 :: Double)
      unless (millis > delayDiff)
        (fail "Internal error: callAndWait ran too quickly!")
                  
      putStrLn "WARNING: callAndWait did not timeout as requested"
      
    Left e -> do
      -- unfortunately there's no guarantee for the timeout error
      putStrLn ("Note: caught (hopefully timeout) error: " ++ show e)
      return () -- TODO: is there a way to check for a timeout?


-- Verify that the subscribed clients to echoMTYPE contain only the
-- supplied ids. This may fail if there are other clients connected
-- to the hub during the test.
--
verifySubscribedClients :: SAMPConnection -> [ClientName] -> Err IO ()
verifySubscribedClients cl ids = do
  rval <- getSubscribedClientsE cl echoMTYPE
  let names = map fst rval
  unless (Set.fromList names == Set.fromList ids)
    (throwError ("MType subscription test (" ++ show echoMTYPE ++ ") " ++
                 "expected=" ++ show ids ++ " " ++
                 "got=" ++ show names))

-- Test callAll and notifyAll
allTests ::
  RandomGen g
  => g -> SAMPConnection -> ResponseMap -> [ClientName] -> Err IO g
allTests gen cl rmap ids = do

  let makeMsg = do
        v4 <- randomSAMPValue 4 False
        let params = M.singleton "val4" v4
        return (v4, toSAMPMessageMT echoMTYPE params extra0)

      ((val4, msg), ngen) = runRand makeMsg gen

      delay = 400 :: Int
      extra0 = M.singleton waitMillisKey (toSValue delay)

      extra1 = M.insert msgIdQueryKey (toSValue (1::Int)) extra0
      tag1 = toMessageTag "anotherTaG99"

      -- No easy way to replace values in a message
      params1 = getSAMPMessageParams msg
      msg1 = toSAMPMessageMT echoMTYPE params1 extra1

      -- It's a programming error if the key does not exist
      -- val4 = fromJust (M.lookup "val4" (getSAMPMessageParams msg))

      head3 = "client 3 callAll: "
      
  notified <- notifyAllE cl msg
  assertSet "Notification list (all)" ids notified

  called <- callAllE cl tag1 msg1
  nreply <- liftIO (countResponse rmap)

  -- This seems like it could be fragile
  when (nreply > 0)
    (putLn "WARNING: Looks like hub call()/notify() not completing quickly")

  forM_ ids $ \rid -> do
    rsp <- liftIO (waitForResponse rmap rid tag1)

    assertTrue (head3 ++ "response okay") (isSAMPSuccess rsp)

    -- if the assert holds then this should be okay
    let params = fromJust (getSAMPResponseResult rsp)
        extra = getSAMPResponseExtra rsp
    
    assert (head3 ++ "val4 value matches")
      (Just val4) (M.lookup "val4" params)

    let mquery = M.lookup msgIdQueryKey extra
    assertTrue (head3 ++ "id=" ++ show rid ++ " has msgId")
      (isJust mquery)

    putLn ("Note: found message id=" ++ show (fromJust mquery))
    
    -- convert to SAMPValue as easier to check than converting the
    -- mquery to MessageId, as that can fail.
    let mid = toSValue <$> lookup rid called
    assert (head3 ++ "id=" ++ show rid ++ " has correct mdgId")
      mid mquery

  -- Finished this section, so there should be no calls
  nreply1 <- liftIO (countResponse rmap)
  assert (head3 ++ "finished processing") 0 nreply1

  waitForProcessing 500
  nreply2 <- liftIO (countResponse rmap)
  assert (head3 ++ "no messages have reappeared") 0 nreply2
  
  return ngen


pingTests ::
  SAMPConnection
  -> ResponseMap
  -> (ClientName, PingCount)  -- client 1
  -> (ClientName, PingCount)  -- client 2
  -> (ClientName, PingCount)  -- client 3
  -> HubTest IO ()
pingTests cl rsp (id1, pc1) (id2, pc2) (_, pc3) = do

  let msg = toSAMPMessageMT pingMTYPE M.empty extra
      extra = M.singleton waitMillisKey (toSValue (100 :: Int))

      getNames = Set.fromList . map fst

      pingHead = "Ping test: "
  
  recipients <- lift (getNames <$> getSubscribedClientsE cl pingMTYPE)
  assertTrue (pingHead ++ " client 1") (Set.member id1 recipients)
  assertTrue (pingHead ++ " client 2") (Set.member id2 recipients)

  -- check that declare subscriptions does not return the calling
  -- client (once it has been subscribed to the message)
  lift (declareSubscriptionsE cl defaultSubs)
  recipients2 <- lift (getNames <$> getSubscribedClientsE cl pingMTYPE)
  assert (pingHead ++ " client 3 not included") recipients recipients2

  -- Send asynchronous messages
  let nPings = 50 :: Int
      pings = [1..nPings]
      toTag = toMessageTag
      showR = fromJust . toRString . show
      tags c = map (\i -> toTag ("abc" ++ showR c ++ "-" ++ showR i)) pings
      
      t1 = tags 1
      t2 = tags 2

  -- unlike HubTester there have already been some counts, so
  -- check the increase (could hard-code the start values, but this
  -- is more modular)
  start1 <- liftIO (getPingCount pc1)
  start2 <- liftIO (getPingCount pc2)
  start3 <- liftIO (getPingCount pc3)
  
  forM_ (zip t1 t2) $ \(tag1, tag2) -> do
    -- NOTE: there's a difference here to TestHub, since here the same
    -- client is sending the message, but in TestHub it's
    --    c3.notify
    --    callable3.call
    --
    lift (notifyE cl id1 msg)
    lift (void (callE cl id1 tag1 msg))

    nlist <- lift (notifyAllE cl msg)
    called <- lift (callAllE cl tag2 msg)

    assert (pingHead ++ " notifyAll recipients") recipients (Set.fromList nlist)
    assert (pingHead ++ " callAll recipients") recipients (getNames called)

  -- wait until all the messages have been received by the clients

  let np1 = start1 + nPings * 4
      np2 = start2 + nPings * 2
      np3 = start3 + 0

      nr3 = nPings * (1 + Set.size recipients)

      spinWait3 = do
        c3 <- getPingCount pc3
        when (c3 < np3) (waitMillis 100 >> spinWait3)
      
      spinWait2 = do
        c2 <- getPingCount pc2
        when (c2 < np2) (waitMillis 100 >> spinWait2)

      spinWait1 = do
        c1 <- getPingCount pc1
        when (c1 < np1) (waitMillis 100 >> spinWait1)

      spinWait = do
        nr <- countResponse rsp
        when (nr < nr3) (waitMillis 100 >> spinWait)
        
  liftIO spinWait3
  liftIO spinWait2
  liftIO spinWait1
  liftIO spinWait
  
  waitForProcessing 400

  -- check for no change
  c1 <- liftIO (getPingCount pc1)
  assert (pingHead ++ " count1") np1 c1

  c2 <- liftIO (getPingCount pc2)
  assert (pingHead ++ " count2") np2 c2

  c3 <- liftIO (getPingCount pc3)
  assert (pingHead ++ " count3") np3 c3

  nr <- liftIO (countResponse rsp)
  assert (pingHead ++ " response") nr3 nr
  
  lift (declareSubscriptionsE cl [])

  -- recipients should not change
  recipients3 <- lift (getNames <$> getSubscribedClientsE cl pingMTYPE)
  assert (pingHead ++ " subs as expected") recipients recipients3

  return ()


errorTests ::
  SAMPConnection -> ClientName -> HubTest IO ()
errorTests cl clId = do
  let msg = toSAMPMessageMT failMTYPE params M.empty
      params = M.singleton errorKey (SAMPMap errMap)
      errMap = M.fromList [ ("samp.errortxt", "failure")
                          , ("samp.code", "999")
                          , ("do.what", "do.that") ]

  rsp <- lift (callAndWaitE cl clId msg Nothing)
  assertTrue "Fail test does fail" (isSAMPError rsp)
  assertTrue "Fail test does fail [redux]" (isSAMPErrorOnly rsp)
  let (errMsg, errVals) = fromJust (getSAMPResponseError rsp)
      errCode = M.lookup "samp.code" errVals
      other = M.lookup "do.what" errVals
  assert "Fail test: reason" "failure" errMsg
  assert "Fail test: code" (Just "999") errCode
  assert "Fail test: other" (Just "do.that") other


-- Send messages which are not being subscribed to. There
-- are a few of these tests dotted throug earlier parts
-- of the test, which I added.
--
subsTest :: SAMPConnection -> ClientName -> ClientName -> HubTest IO ()
subsTest cl clId1 clId2 = do

  let mtype = "not.an.mtype"
      msg = toSAMPMessageMT mtype M.empty M.empty

  subs <- lift (map fst <$> getSubscribedClientsE cl mtype)
  assertTrue "Unknown MTYPE: client 1" (clId1 `notElem` subs)
  assertTrue "Unknown MTYPE: client 2" (clId2 `notElem` subs)

  let xtag = toMessageTag "xxx"
      ytag = toMessageTag "yyy"
  checkFail "Unkown MTYPE: notify" (notifyE cl clId1 msg)
  checkFail "Unknown MTYPE: call" (void (callE cl clId1 xtag msg))
  checkFail "Unknown MTYPE: callAndWait" (void (callAndWaitE cl clId1 msg Nothing))

  list <- lift (notifyAllE cl msg)
  assert "Notify All of unsubscribed message" [] list

  called <- lift (callAllE cl ytag msg)
  assert "Call All of unsubscribed message" [] called
  

  

testClients :: SAMPInfo -> HubTest IO ()
testClients si = do

  withClient (makeClient si) testClient0
  
  cl1 <- makeClient si
  mdata1 <- lift (setupClient1 cl1)
  assertTestClients cl1 []
  
  cl2 <- makeClient si
  mdata2 <- lift (setupClient2 cl2)
  
  let clId1 = scId cl1
      clId2 = scId cl2
  assertTestClients cl2 [clId1]
  assertTestClients cl1 [clId2]

  gmeta11 <- lift (getMetadataE cl1 clId1)
  assertSAMPMap "meta data client 1 (conn cl1)" mdata1 gmeta11
  
  gmeta21 <- lift (getMetadataE cl2 clId1)
  assertSAMPMap "meta data client 1 (conn cl2)" mdata1 gmeta21

  gmeta12 <- lift (getMetadataE cl1 clId2)
  assertSAMPMap "meta data client 2 (conn cl1)" mdata2 gmeta12
  
  gmeta22 <- lift (getMetadataE cl2 clId2)
  assertSAMPMap "meta data client 2 (conn cl2)" mdata2 gmeta22

  drink <- assertKey "Client 1 meta data" "test.drink" gmeta21
  assert "test.drink from client 1" "cider" drink

  let mdata1' = M.insert "test.drink" "scrumpy" mdata1
  lift (declareMetadataE cl1 mdata1')

  gmeta11' <- lift (getMetadataE cl1 clId1)
  assertSAMPMap "meta data client 1 (conn cl1)" mdata1' gmeta11'
  
  gmeta21' <- lift (getMetadataE cl2 clId1)
  assertSAMPMap "meta data client 1 (conn cl2)" mdata1' gmeta21'

  drink' <- assertKey "Client 1 meta data (replaced)" "test.drink" gmeta21'
  assert "test.drink from client 1 (replaced)" "scrumpy" drink'

  -- for now rmap1 is not used
  (pingCount1, _, tid1) <- liftIO (callableClient cl1)

  let subs1 = [ ("test.dummy.1", emptyMap)
              , ("test.dummy.2", emptyMap) ]
  lift (declareSubscriptionsE cl1 subs1)

  gsub11 <- lift (getSubscriptionsE cl1 clId1)

  let assertSubs lbl vs1 vs2 = do
        -- A valid MType is a valid RString
        let conv = fromJust . toRString . fromMType
            convL = M.fromList . map (first conv)
            xs1 = convL vs1
            xs2 = convL vs2
        assertSAMPMap lbl xs1 xs2
        
  assertSubs "subscriptions client 1 (conn cl1)" subs1 gsub11
  
  gsub21 <- lift (getSubscriptionsE cl2 clId1)
  assertSubs "subscriptions client 1 (conn cl2)" subs1 gsub21

  let d3atts = M.fromList [("size", "big"), ("color", "blue")]
      subs1' = ("test.dummy.3", SAMPMap d3atts) : subs1

  lift (declareSubscriptionsE cl1 subs1')

  gsub21' <- lift (getSubscriptionsE cl2 clId1)
  assertSubs "subscriptions client 1 (conn cl2) after addition"
    subs1' gsub21'

  -- HubTester explicitly checks that the "test.dummy.1"/"3" messages
  -- have empty hash/hash with size->big in it. However, this is
  -- effectively already tested with the comparison to subs1' above,
  -- so do not bother with the explicit checks here.

  lift (declareSubscriptionsE cl1 defaultSubs)

  (pingCount2, rmap2, tid2) <- liftIO (callableClient cl2)
  lift (declareSubscriptionsE cl2 defaultSubs)

  -- TODO: check for error
  --  - could come up with a random id that we know is not
  --    valid, but not worth it (and can not guarantee some other
  --    client doesn't register it)
  let errAct1 = getMetadataE cl1 "This ID is not valid" >> return False
  flag1 <- lift (errAct1 `catchError` (const (return True)))
  assertTrue "Error out when query metadata with an unregistered ID" flag1

  let errAct2 = getSubscriptionsE cl1 "This ID is not valid" >> return False
  flag2 <- lift (errAct2 `catchError` (const (return True)))
  assertTrue "Error out when query subscriptions with an unregistered ID" flag2

  -- HubTester re-uses a tag for this client: that is, the message tag can
  -- be re-used. This is not something that is checked for in the HSAMP
  -- API (it is assumed that message ids are not re-used), so just keep the
  -- client in
  cl4 <- makeClient si
  (pingCount4, rmap4, tid4) <- liftIO (callableClient cl4)
  
  let clId4 = scId cl4
  assertTestClients cl2 [clId1, clId4]
  assertTestClients cl1 [clId2, clId4]
  assertTestClients cl4 [clId1, clId2]

  putLn "DBG: about to send messages"

  -- Now the fun begins: send echo messages via notify and call

  -- TODO: allow the msgIdQuery value to be optional
  let makeMsg :: RandomGen g => Bool -> Int -> Rand g SAMPMessage
      makeMsg qFlag waitTime = do
        val1 <- randomSAMPValue 2 False
        val2 <- randomSAMPValue 4 False
        let params = M.fromList [ ("val1", val1)
                                , ("val2", val2)
                                , ("val3", toSValue sval) ]

            -- TODO: should make this a top-level symbol
            sval = case toEitherE (toRStringE legalSAMPChars) of
              Right rstr -> rstr
              Left emsg -> error ("Internal error: legalSAMPChars: " ++ emsg)
        
            extra = M.fromList [ (waitMillisKey, toSValue waitTime)
                               , (msgIdQueryKey, toSValue qFlag) ]

        return (toSAMPMessageMT echoMTYPE params extra)

  gen <- liftIO newStdGen  -- should this be a fixed value for repeatability?
  let necho = 5
      echos = [0..(necho-1)]
      waitTimes = map (\i -> 200 + 100 * i) echos
      conv s = case toRString s of
        Just rstr -> toMessageTag rstr
        Nothing -> error ("Internal error: invalid messageTag: " ++ s)
        
      tags = map (\i -> conv ("tag" ++ show i)) echos
      makeMsgs = mapM (makeMsg True) waitTimes

      sameTag = toMessageTag "sametag"
      
      (msgs, gen2) = runRand makeMsgs gen

  let procMsg (msg, tag) = do
        notifyE cl2 clId1 msg
        msgId2 <- callE cl2 clId1 tag msg
        msgId4 <- callE cl4 clId1 sameTag msg
        return (msgId2, msgId4)
        
  ans <- lift (mapM procMsg (zip msgs tags))
  let (msgIds2, msgIds4) = unzip ans

  -- check to see if the call and notify methods are being
  -- processed "quickly". I am not 100% sure of the logic
  -- here; I think it's saying that if we get here and a
  -- lot of the calls have been processed then it
  -- means that the overhad of making the calls is too large.
  --
  check <- liftIO (countResponse rmap2)
  when (check > necho `div` 2)
    (putLn ("WARNING: looks like hub call/notify not completing " ++
            "quickly (" ++ show check ++ "/" ++ show necho ++ ")"))
  
  let spinWait = do
        c2 <- countResponse rmap2
        c4 <- countResponse rmap4
        unless (c2 == necho && c2 == c4) (waitMillis 100 >> spinWait)

  liftIO spinWait
        
  let findId rsp = findKey msgIdQueryKey (getSAMPResponseExtra rsp)

  -- Validate results from client 2
  forM_ (zip3 msgs tags msgIds2) $ \(msg, msgTag, msgId) -> do
    let head2 = "Client 2 from=" ++ show clId1 ++ " tag=" ++ show msgTag

    rsps <- liftIO (removeResponse rmap2 clId1 msgTag)
    case rsps of
      [rsp] -> do
        assertTrue (head2 ++ " response okay") (isSAMPSuccess rsp)

        let params = getSAMPMessageParams msg
        case getSAMPResponseResult rsp of
          Just res -> assertSAMPMap (head2 ++ " response") params res
          _ -> throwError (head2 ++ " internal error: no result!")

        case findId rsp of
          Just msgKey -> assert (head2 ++ " msgId is correct") msgId msgKey
          _ -> throwError (head2 ++ " key missing: " ++ show msgIdQueryKey)

      [] -> throwError (head2 ++ " no responses")
      _ -> throwError (head2 ++ " multiple responses: " ++ show (length rsps))

  -- Validate results from client 4
  rsps4 <- liftIO (removeResponse rmap4 clId1 sameTag)
  let head4 = "Client 4 from id=" ++ show clId1 ++ " tag=" ++ show sameTag
  assert (head4 ++ " number of responses") necho (length rsps4)
  assertTrue (head4 ++ " responses okay") (all isSAMPSuccess rsps4)

  -- This does not appear to be checked in HubTester
  case mapM findId rsps4 of
    Just gotIds4 -> assert (head4 ++ " ids are wrong")
                    (sort msgIds4) (sort gotIds4)
    Nothing -> throwError (head4 ++ " missing an id in a response")

  nrsp2 <- liftIO (countResponse rmap2)
  assert "No more responses for client 2" 0 nrsp2

  nrsp4 <- liftIO (countResponse rmap4)
  assert "No more responses for client 4" 0 nrsp4
  
  -- check the ping count: (check not in TestHub)
  assertPing "Client 1" 0 pingCount1
  assertPing "Client 2" 0 pingCount2
  assertPing "Client 4" 0 pingCount4

  let pingMsg = toSAMPMessageMT pingMTYPE M.empty M.empty
  matches <- lift (notifyAllE cl4 pingMsg)

  -- validate matches
  assertTrue "client 1 was pinged" (clId1 `elem` matches)
  assertTrue "client 2 was pinged" (clId2 `elem` matches)
  assertTrue "client 4 was not pinged" (clId4 `notElem` matches)

  -- no obvious way to wait for all the notifications to be
  -- processed, so hard code a delay
  waitForProcessing 400

  assertPing "Client 1" 1 pingCount1
  assertPing "Client 2" 1 pingCount2
  assertPing "Client 4" 0 pingCount4
  
  -- TODO: how to make sure clients are unregistered on error?
  removeClient cl4 tid4
  putLn ("Finished closing down client 4")

  -- callAndWait testing

  let waitTimes2 = map (\i -> 100 * i) echos
      makeMsgs2 = mapM (makeMsg False) waitTimes2

      (msgs2, gen3) = runRand makeMsgs2 gen2

  -- No time out here
  forM_ msgs2 $ \msg -> do
    rsp <- lift (callAndWaitE cl2 clId1 msg Nothing)

    let head2 = "callAndWait client 2 calling id=" ++ show clId1
    assertTrue (head2 ++ " response okay") (isSAMPSuccess rsp)
    
    let params = getSAMPMessageParams msg
    case getSAMPResponseResult rsp of
      Just res -> assertSAMPMap (head2 ++ " response") params res
      _ -> throwError (head2 ++ " internal error: no result!")

    -- should a test be made of the extra parameters?

  -- forM_ [0..5::Int] $ \ie -> do
  forM_ [0..3::Int] $ \ie -> do  -- use a max of 3 for testing for now
                                 -- as larger values take too long
    let num = 10 ^ ie
        xs = toSValue (map toSValue [1 .. num::Int])
        pars = M.singleton "list" xs
        msg = toSAMPMessageMT echoMTYPE pars M.empty

    putLn ("call and wait with list size " ++ show num)
    rsp <- lift (callAndWaitE cl2 clId1 msg Nothing)
    -- NOTE: hsamp-hub does not time out correctly for long messages 
    
    let head2 = "callAndWait client 2 calling id=" ++ show clId1 ++
                " ie=" ++ show ie
    assertTrue (head2 ++ " response okay") (isSAMPSuccess rsp)
    
    case getSAMPResponseResult rsp of
      Just res ->
        case M.lookup "list" res of
          Just list -> assert (head2 ++ " contains list") xs list
          _ -> throwError (head2 ++ " has no list element")

      Nothing -> throwError (head2 ++ " internal error: no result!")

  -- test the timeout; this is not required behavior so only warn
  -- if it fails
  liftIO (validateTimeOut cl2 clId1)

  cl3 <- makeClient si
  let clId3 = scId cl3
  (pingCount3, rmap3, tid3) <- liftIO (callableClient cl3)

  let otherIds = [clId1, clId2]
  lift (verifySubscribedClients cl3 otherIds)

  -- TODO: for now ignore the new generator
  _ <- lift (allTests gen3 cl3 rmap3 otherIds)

  pingTests cl3 rmap3
    (clId1, pingCount1) (clId2, pingCount2) (clId3, pingCount3)
  
  -- Client 3 is not registered to a ping message so this should
  -- be an error.
  let call = notifyE cl1 clId3 (toSAMPMessageMT pingMTYPE M.empty M.empty) 
  flag <- lift ((call >> return False) `catchError` (\_ -> return True))
  assertTrue "Client 3 is not subscribed to ping" flag

  errorTests cl3 clId2
  subsTest cl3 clId1 clId2

  -- tests on client 0 which has been unregistered; need to review

{-
        // Check that hub event messages arrived concerning client 0 which
        // we registered and unregistered earlier.  Do it here to give 
        // messages enough time to have arrived; SAMP offers no guarantees
        // of delivery sequence, but if they haven't showed up yet it's 
        // very likely that they never will.
        Throwable cwError = clientWatcher_.getError();
        if ( cwError != null ) {
            throw new TestException( "Error encountered during hub event " 
                                   + "processing", cwError );
        }
        WatchedClient client0 = clientWatcher_.getClient( id0 );
        assertTrue( client0 != null );
        assertTrue( client0.reg_ );
        assertTrue( client0.unreg_ );
        assertEquals( meta0, client0.meta_ );
        assertEquals( subs0, client0.subs_ );

        // Check that the client watcher has received hub event messages 
        // concerning itself as well.
        String cwId = clientWatcher_.getConnection().getRegInfo().getSelfId();
        WatchedClient cwClient = clientWatcher_.getClient( cwId ); 
        assertTrue( cwClient != null );
        assertTrue( ! cwClient.unreg_ );
        assertEquals( clientWatcher_.getMetadata(), cwClient.meta_ );
        assertEquals( clientWatcher_.getSubscriptions(), cwClient.subs_ );
-}

  assertTestClients cl1 [clId2, clId3]
  assertTestClients cl2 [clId1, clId3]
  removeClient cl3 tid3
  putLn ("Finished closing down client 3")
  
  assertTestClients cl1 [clId2]
  assertTestClients cl2 [clId1]
  
  -- unregister them in a different order to those are created
  removeClient cl1 tid1
  putLn ("Finished closing down client 1")
  
  removeClient cl2 tid2
  putLn ("Finished closing down client 1")

  -- check that unregistering an unregistered client is an error
  checkFail "Client already unregistered" (unregisterE cl1)
  

testStress :: SAMPInfo -> HubTest IO ()
testStress _ = do
  putLn "Stress tests to be written"
  
