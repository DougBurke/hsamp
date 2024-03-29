{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2015, 2016, 2018, 2022
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------
-}

{-
Utilities for the hub-testing code.
-}

module TestUtils (
  HubTest

  -- * information about the hub during testing
  
  , HubStore(..)
  , emptyHubStore
    
    -- * assertions
  , assert
  , assertKey
  , assertNoKey
  , assertPing
  , assertSAMPMap
  , assertSet
  , assertTestClients
  , assertTrue
  , checkFail
    
    -- * client/registration stuff
  -- , addClient
  , addOtherClients
  , makeClient
  , makeClients
  , closeClient
  , closeClients
  , removeClient
  -- , removeClient_

    -- * other stuff
  , atomicallyIO
  , mapConcurrentlyE
  , putLn
    
    -- * counters
  , EvalCounter
  , PingCounter
    
  , getCounter
  , increaseCounter

  , newEvalCounter
  , newPingCounter

    -- * store
  , Store
  , addStore
  , getStore
  , newStore
  , nullStore
    
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar,
                                    writeTVar)

import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (ExceptT, catchError, throwError)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.STM (STM, atomically)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', put)

import Data.Maybe (catMaybes)

import Network.SAMP.Standard (Err, SAMPConnection(..), SAMPInfo)
import Network.SAMP.Standard.Client (getRegisteredClientsE
                                    , getMetadataE
                                    , getSubscriptionsE
                                    , registerClientE
                                    , unregisterE)
import Network.SAMP.Standard.Types (ClientName, ClientSecret
                                   , SAMPValue(SAMPMap), SAMPMapValue
                                   , dumpSAMPValue, runE)

import Utils (waitForProcessing)

-- | Lift a STM transaction.
atomicallyIO :: MonadIO m => STM a -> m a
atomicallyIO = liftIO . atomically

-- transformer stack: layer State on top of Err
--
-- NOTE: this means that I can't use the _hsRunning field
-- in HubStore to unregister clients on error because
-- for this to work it would need to be
--
--   type HubTest m a = ExceptT String (StateT HubStore) a
--
type HubTest m a = StateT HubStore (ExceptT String m) a


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
  , _hsRunning :: [SAMPConnection]
    -- ^ The list of running clients. Should be a set but do not want
    --   an Ord constraint on SAMPConnection, and going to a map (with
    --   ClientName as the key) seems to much.
  }


emptyHubStore :: HubStore
emptyHubStore = 
  HubStore
  { 
    _hsOtherClients = []
  , _hsUsedNames = []
  , _hsUsedSecrets = []
  , _hsRunning = []
  }


-- Overwrite the list of client names
addOtherClients :: Monad m => [ClientName] -> HubTest m ()
addOtherClients names = 
  modify' (\s -> s { _hsOtherClients = names })


-- A new client has been created, so check it doesn't clash
-- (name or secret) with previous clients and add it to the
-- store.
--
-- Also does some simple validation, that is, checks that
-- the clent has no metadata or siubscriptions.
--
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
removeClient cl tid = removeClient_ cl tid >> waitForProcessing 100

-- | This is 'removeClient' but without the delay
removeClient_ :: SAMPConnection -> ThreadId -> HubTest IO ()
removeClient_ cl tid = do
  putLn ("Removing client: " ++ show (scId cl))
  closeClient cl
  liftIO (killThread tid)


-- | Unregisters the client  and removes itself from the hubtest
--   state (if known).
--
closeClient :: SAMPConnection -> HubTest IO ()
closeClient cl = do
  lift (unregisterE cl)
  let clean = filter (\conn -> scId conn /= scId cl)
  modify' (\s -> s { _hsRunning = clean (_hsRunning s) })

-- | Asynchronously unregister the clients, then remove them
--   from the hubtest state (if known).
--
closeClients :: [SAMPConnection] -> HubTest IO ()
closeClients cls = do
  void (lift (mapConcurrentlyE unregisterE cls))
  let names = map scId cls
      clean = filter (\conn -> scId conn `notElem` names)
  modify' (\s -> s { _hsRunning = clean (_hsRunning s) })
  

-- | Create multiple clients (and basic tests) making the
--   process as asynchronous as possible.
--
--   Since this is just a thin wrapper around 'makeClients',
--   it is not as efficient as it could be, but speed is not
--   an issue here.
makeClient :: SAMPInfo -> HubTest IO SAMPConnection
makeClient si = head `fmap` makeClients si 1


-- | Run the actions concurrently and wait for their
--   results.
--
--   If the action fails then it will not be
--   caught by `mapConcurrently` (i.e. will not be
--   converted to `Err IO`), but will raise an exception.
--   This should probably be changed.
--
mapConcurrentlyE :: (a -> Err IO b) -> [a] -> Err IO [b]
mapConcurrentlyE f = liftIO . mapConcurrently (runE . f)


-- | Create multiple clients (and basic tests) making the
--   process as asynchronous as possible.
--
makeClients ::
  SAMPInfo
  -> Int  -- ^ The number of clients to make; assumed to be > 0
  -> HubTest IO [SAMPConnection]
makeClients si nclient = do

  when (nclient < 1)
    (throwError ("Internal error: nclient=" ++ show nclient))
  
  let cts = [1 .. nclient]
      register _ = registerClientE si
  conns <- lift (mapConcurrentlyE register cts)

  -- separated from the registration step for simplicity
  modify' (\s -> s { _hsRunning = conns ++ _hsRunning s })

  let showClient conn = 
        let clientName = scId conn
            clientSecret = scPrivateKey conn
        in putLn ("Created client: " ++ show clientName ++ " secret: " ++
                  show clientSecret)
  mapM_ showClient conns

  let clientNames = map scId conns
      getMetadata = uncurry getMetadataE
      clArgs = zip conns clientNames

  mds <- lift (mapConcurrentlyE getMetadata clArgs)
  forM_ mds (assertTrue "New client has no metadata" . M.null)

  let getSubs = uncurry getSubscriptionsE
  subs <- lift (mapConcurrentlyE getSubs clArgs)
  forM_ subs (assert "New client has no subscriptions" [])

  -- verify that a client is not included in its own "registered clients"
  -- list.
  let checkReg (conn, clientName) = do
        rclients <- getRegisteredClientsE conn
        if clientName `elem` rclients
          then return (Just clientName)
          else return Nothing

  mfails <- lift (mapConcurrentlyE checkReg clArgs)
  let fails = catMaybes mfails
  assert "No client is in its own registered client list" [] fails

  -- Ensure the store knows about the clients (and verify that
  -- they are not re-using previous names)
  forM_ conns addClient
  return conns


-- | Increase a counter (limited to Int, assumed to be
--   a non-negative integer).
--
--   The phantom type is used to distinguish between the different
--   type of counters.
--
newtype Counter a = Ctr { _unCtr :: TVar Int }

data EvalCounterType
data PingCounterType

type EvalCounter = Counter EvalCounterType
type PingCounter = Counter PingCounterType

newCounter :: IO (Counter a)
newCounter = Ctr <$> newTVarIO 0

-- | Counter for number of times the calculator has been evaluated
--   (e.g. called with notify or call).
newEvalCounter :: IO EvalCounter
newEvalCounter = newCounter


-- | The number of times a ping call was received.
newPingCounter :: IO PingCounter
newPingCounter = newCounter


increaseCounter :: Counter a -> STM ()
increaseCounter (Ctr c) = modifyTVar' c succ


getCounter :: Counter a -> STM Int
getCounter = readTVar . _unCtr


-- | A basic key-value store
type Store k a = TVar (M.Map k a)

newStore :: IO (Store k a)
newStore = newTVarIO M.empty

-- | The assumption is that the key does not exist in the
--   store, but this is not enforced (at least for now).
--
addStore :: Ord k => k -> a -> Store k a -> STM ()
addStore key val store = modifyTVar' store (M.insert key val)


-- | This removes the entry from the store, if it exists
--
getStore :: Ord k => k -> Store k a -> STM (Maybe a)
getStore key store = do
  oldVal <- readTVar store
  let f _ _ = Nothing
      (rval, newVal) = M.updateLookupWithKey f key oldVal
  writeTVar store newVal
  return rval
    

-- | Is the store empty
nullStore :: Store k a -> STM Bool
nullStore s = M.null <$> readTVar s


-- assertions

assertTrue :: MonadError a m => a -> Bool -> m ()
assertTrue lbl f = unless f (throwError lbl)


checkFail :: String -> Err IO a -> HubTest IO ()
checkFail lbl call = do
  flag <- lift ((call >> return False) `catchError` (\_ -> return True))
  assertTrue ("Expected fail: " ++ lbl) flag



assert ::
  (MonadError String m, Eq a, Show a) => String -> a -> a -> m ()
assert lbl expVal gotVal =
  let lbl2 = lbl ++ ":\nexpected=" ++ show expVal ++
             "\ngot     =" ++ show gotVal
  in assertTrue lbl2 (expVal == gotVal)

-- An unordered comparison of the two lists
--
-- Eq a is warned as being redundant in ghc 8.0
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
  when (any ((== k) . fst) kvs)
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
assertPing :: String -> Int -> PingCounter -> HubTest IO ()
assertPing lbl count pc = do
  got <- atomicallyIO (getCounter pc)
  assert ("Ping count: " ++ lbl) count got


putLn :: MonadIO m => String -> m ()
putLn = liftIO . putStrLn

