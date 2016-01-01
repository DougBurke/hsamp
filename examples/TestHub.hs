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

  ./testhub --debug [rng1 rng2]

The rng1/2 arguments are to allow problematic generators to
be re-run, but it looks like the storm test failures are
non-deterministic, so this isn't that useful.

To do:

Problems include

 a) the test of long messages is currently limited to 0..3 rather
    than 0..5 since the latter is *very* slow with hsamp-hub
 b) the storm tests can hang

-}

module Main (main) where

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Control.Arrow (first)

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar,
                                    writeTVar)

import Control.Monad (forM_, replicateM, unless, when, void)
import Control.Monad.Except (catchError, throwError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random (MonadRandom(..), Rand, runRand, uniform)
import Control.Monad.STM (STM, atomically, retry)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (execStateT)

import Data.Char (chr)
import Data.List (isPrefixOf, partition, sort)
import Data.Maybe (fromJust, isJust)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- import Data.Version (showVersion)

import Network.SAMP.Standard ( SAMPInfo, SAMPConnection(..)
                             , Err
                             , handleError
                             , getSAMPInfoHubURL, getSAMPInfoHubMetadata
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
                                    , toMetadata
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
                                   , MessageId
                                   , MessageTag
                                   , MType
                                   , RString
                                   , SAMPMapValue
                                   , SAMPMessage
                                   , SAMPMapElement
                                   , SAMPResponse
                                   , SAMPType
                                   , SAMPValue(..)
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
import System.Random (RandomGen, StdGen, newStdGen)

import Text.Read (readEither)

-- import Paths_hsamp (version)

import Calculator (runCalcStorm)
import TestUtils (HubStore(..), HubTest, PingCounter
                 , assert, assertKey, assertNoKey, assertPing
                 , assertSAMPMap, assertSet, assertTestClients, assertTrue
                 , checkFail
                 , emptyHubStore
                 , addOtherClients, makeClient, removeClient
                 , putLn
                 , getCounter, increaseCounter, newPingCounter)
import Utils (getAddress, getSocket, waitMillis, waitForProcessing)

-- | Lift a STM transaction.
atomicallyIO :: MonadIO m => STM a -> m a
atomicallyIO = liftIO . atomically

-- | Store the responses to a client, labelled by the message
--   tag.
--
type ResponseMapSTM = TVar [((ClientName, MessageTag), SAMPResponse)]

newResponseMapSTM :: IO ResponseMapSTM
newResponseMapSTM = newTVarIO []

countResponseSTM :: ResponseMapSTM -> STM Int
countResponseSTM rmap = length `fmap` readTVar rmap

-- | Add the response to the start of the list.
--
addResponseSTM ::
  ResponseMapSTM -> ClientName -> MessageTag -> SAMPResponse -> STM ()
addResponseSTM rmap clId msgTag rsp =
  let newVal = ((clId, msgTag), rsp)
      add kvs = newVal : kvs
  in modifyTVar' rmap add


-- | Removes all entries that match this client and tag. The return value
--   can be empty.
--
removeResponseSTM ::
  ResponseMapSTM -> ClientName -> MessageTag -> STM [SAMPResponse]
removeResponseSTM rmap clId msgTag = do
  oldVals <- readTVar rmap
  let match (k,_) = k == (clId, msgTag)
      (xs, newVals) = partition match oldVals
  writeTVar rmap newVals
  return (map snd xs)


-- | Wait until the given response has been returned. It
--   returns a single response, even if there are multiple
--   ones in the store.
--
--   This is a simple loop and there is no time out.
--
waitForResponseSTM ::
  ResponseMapSTM -> ClientName -> MessageTag -> STM SAMPResponse
waitForResponseSTM rmap clId msgTag = do
  let key = (clId, msgTag)

      -- I wanted to just read the map in this loop, but there's
      -- a potential race if the extraction step is separate from
      -- the check step.
      --
      -- Only interested in the first key in this case
      getKV kvs = case break ((== key) . fst) kvs of
        (_, []) -> Nothing
        (l, h:r) -> Just (l ++ r, snd h)

  oldVals <- readTVar rmap
  case getKV oldVals of
    Nothing -> retry
    Just (newVals, ans) -> do
      writeTVar rmap newVals
      return ans

    
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
  chars <- replicateM nchar (uniform fromChars)
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

  els <- replicateM nel (randomSAMPValue pLevel anyPrint)
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
      
  keys <- replicateM nel (randomSAMPString anyPrint)
  els <- replicateM nel (randomSAMPValue pLevel anyPrint)
  return (SAMPMap (M.fromList (zip (map convKey keys) els)))


toMaybeE :: Err Maybe a -> Maybe a
toMaybeE = handleError (const Nothing)

toEitherE :: Err (Either String) a -> Either String a
toEitherE = handleError Left


usage :: IO ()
usage = do
  pName <- getProgName
  hPutStrLn stderr ("Usage: " ++ pName ++ " --debug [rng1 rng2]")
  exitFailure


main :: IO ()
main = do
  allArgs <- getArgs
  let (opts, args) = partition ("--" `isPrefixOf`) allArgs
  case opts of
    [] -> return ()
    ["--debug"] -> updateGlobalLogger "SAMP" (setLevel DEBUG)
    _ -> usage

  gen <- case args of
    [] -> newStdGen
    [gen1, gen2] -> do
      let gstr = gen1 ++ " " ++ gen2
      case readEither gstr of
        Right g -> return g
        Left emsg -> do
          putStrLn ("Unable to parse " ++ gstr ++ ": " ++ emsg)
          usage >> newStdGen -- ugh
          
    _ -> usage >> newStdGen -- ugh
    
  withSAMP (runHubTests (runTests gen))
  exitSuccess


runHubTests :: HubTest IO a -> IO ()
-- runHubTests act = void (runE (evalStateT act emptyHubStore))
runHubTests act = do
  newState <- runE (execStateT act emptyHubStore)
  putStrLn "Final state:"
  putStrLn ("  other clients: " ++ show (_hsOtherClients newState))
  putStrLn ("  used names   : " ++ show (_hsUsedNames newState))
  putStrLn ("  used secrets : " ++ show (_hsUsedSecrets newState))

  


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


runTests :: StdGen -> HubTest IO ()
runTests gen = do
  hubUrl <- lift sampLockFileE
  putLn ("Hub location: " ++ show hubUrl)
  hi <- lift (getHubInfoE hubUrl)
  liftIO (reportHub hi)
  
  let genStr = show gen

  withClient (makeClient hi) basicClient
  gen1 <- testClients gen hi
  putLn "*** starting calculator tests ***"
  _ <- runCalcStorm genStr gen1 hi 10 20
  
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
  -> IO (PingCounter, ResponseMapSTM, ThreadId)
callableClient conn = do
  mvar <- newEmptyMVar
  pingCount <- newPingCounter
  responseMap <- newResponseMapSTM

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


  
-- Routines to handle SAMP conversations from a hub to a client

-- samp.client.receiveNotification
notifications ::
  PingCounter
  -> SAMPNotificationMap
notifications pingCount =
  [ (echoMTYPE, ignoreEcho)
  , (pingMTYPE, notifyPing pingCount)
  , ("*", unexpectedNotification)
  ]


ignoreEcho :: SAMPNotificationFunc
ignoreEcho _ _ _ = return ()

notifyPing :: PingCounter -> SAMPNotificationFunc
notifyPing pc _ clName _ = do
  putStrLn ("SENT A PING (notify from " ++ show clName ++ ")") -- DBG
  increaseCounter pc

dumpKV :: (Show a, Show b) => (a, b) -> IO ()
dumpKV (k,v) = putStrLn ("    key=" ++ show k ++ " value=" ++ show v)

extract :: SAMPMessage -> (MType, [SAMPMapElement], [SAMPMapElement])
extract msg = 
  let mtype = getSAMPMessageType msg
      keys = M.toList (getSAMPMessageParams msg)
      extra = M.toList (getSAMPMessageExtra msg)
  in (mtype, keys, extra)

     
unexpectedNotification :: SAMPNotificationFunc
unexpectedNotification _ clName msg = do
  reportUnexpected clName "receiveNotification" Nothing msg
  error "SHOULD NOT HAVE HAPPENED"
  

reportUnexpected ::
  ClientName
  -> String   -- ^ label
  -> Maybe String  -- ^ extra info
  -> SAMPMessage
  -> IO ()
reportUnexpected clName label mstr msg = do
  let (mtype, keys, extra) = extract msg
  putStrLn ("Error: client " ++ show clName ++ " " ++
            label ++ "=" ++ show mtype)
  case mstr of
    Just str -> putStrLn str
    _ -> return ()
  putStrLn "  keys:"
  forM_ keys dumpKV
  unless (null extra) (putStrLn "  extra:" >> forM_ extra dumpKV)
  putStrLn ("<-- end " ++ label ++ " -->")
  

-- samp.client.receiveCall
calls :: PingCounter -> SAMPCallMap
calls pingCount =
  [ (echoMTYPE, callEcho)
  , (pingMTYPE, callPing pingCount)
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


callPing :: PingCounter -> SAMPCallFunc
callPing pc _ _ msgId msg = do
  delayCall msg
  increaseCounter pc
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
  let txt = " msg id: " ++ show msgId
  reportUnexpected clName "receiveCall" (Just txt) msg
  delayCall msg
  let other = addMessageId msgId msg
  return (toSAMPResponseError
          "Client is not subscribed to this MType)" M.empty other)


-- samp.client.receiveResponse
--
-- TODO: use a mvar or channel?
rfunc ::
  ResponseMapSTM
  -> SAMPResponseFunc
{-
rfunc rmap secret clid msgTag rsp = do
  putStrLn ("samp.client.receiveResponse called: id=" ++ show clid ++
            " secret=" ++ show secret ++ " msg-tag=" ++ show msgTag ++
            "\nresponse=" ++ show rsp) -- DBG
  addResponse rmap msgTag rsp
-}    
rfunc rmap _ clId msgTag rsp = atomically (addResponseSTM rmap clId msgTag rsp)


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
  => g -> SAMPConnection -> ResponseMapSTM -> [ClientName] -> Err IO g
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
  nreply <- atomicallyIO (countResponseSTM rmap)

  -- This seems like it could be fragile
  when (nreply > 0)
    (putLn "WARNING: Looks like hub call()/notify() not completing quickly")

  forM_ ids $ \rid -> do
    rsp <- atomicallyIO (waitForResponseSTM rmap rid tag1)

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
  nreply1 <- atomicallyIO (countResponseSTM rmap)
  assert (head3 ++ "finished processing") 0 nreply1

  waitForProcessing 500
  nreply2 <- atomicallyIO (countResponseSTM rmap)
  assert (head3 ++ "no messages have reappeared") 0 nreply2
  
  return ngen


pingTests ::
  SAMPConnection
  -> ResponseMapSTM
  -> (ClientName, PingCounter)  -- client 1
  -> (ClientName, PingCounter)  -- client 2
  -> (ClientName, PingCounter)  -- client 3
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
  start1 <- liftIO (getCounter pc1)
  start2 <- liftIO (getCounter pc2)
  start3 <- liftIO (getCounter pc3)
  
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
        c3 <- getCounter pc3
        when (c3 < np3) (waitMillis 100 >> spinWait3)
      
      spinWait2 = do
        c2 <- getCounter pc2
        when (c2 < np2) (waitMillis 100 >> spinWait2)

      spinWait1 = do
        c1 <- getCounter pc1
        when (c1 < np1) (waitMillis 100 >> spinWait1)

      spinWait = do
        nr <- countResponseSTM rsp
        when (nr < nr3) retry
        
  liftIO spinWait3
  liftIO spinWait2
  liftIO spinWait1
  atomicallyIO spinWait
  
  waitForProcessing 400

  -- check for no change
  c1 <- liftIO (getCounter pc1)
  assert (pingHead ++ " count1") np1 c1

  c2 <- liftIO (getCounter pc2)
  assert (pingHead ++ " count2") np2 c2

  c3 <- liftIO (getCounter pc3)
  assert (pingHead ++ " count3") np3 c3

  nr <- atomicallyIO (countResponseSTM rsp)
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
  

-- Validate results from client 2
validateClient2 ::
  (Eq a, Show a, MonadIO m)
  => (SAMPResponse -> Maybe a)
  -> ClientName
  -> ResponseMapSTM
  -> [SAMPMessage]
  -> [MessageTag]
  -> [a]
  -> Err m ()
validateClient2 findId clId1 rmap2 msgs tags msgIds2 =
  forM_ (zip3 msgs tags msgIds2) $ \(msg, msgTag, msgId) -> do
    let head2 = "Client 2 from=" ++ show clId1 ++ " tag=" ++ show msgTag

    rsps <- atomicallyIO (removeResponseSTM rmap2 clId1 msgTag)
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
validateClient4 ::
  (Ord a, Show a, MonadIO m)
  => (SAMPResponse -> Maybe a)
  -> ClientName
  -> ResponseMapSTM
  -> MessageTag
  -> [a]
  -> Int
  -> Err m ()
validateClient4 findId clId1 rmap4 sameTag msgIds4 necho = do
  rsps4 <- atomicallyIO (removeResponseSTM rmap4 clId1 sameTag)
  let head4 = "Client 4 from id=" ++ show clId1 ++ " tag=" ++ show sameTag
  assert (head4 ++ " number of responses") necho (length rsps4)
  assertTrue (head4 ++ " responses okay") (all isSAMPSuccess rsps4)

  -- This does not appear to be checked in HubTester
  case mapM findId rsps4 of
    Just gotIds4 -> assert (head4 ++ " ids are wrong")
                    (sort msgIds4) (sort gotIds4)
    Nothing -> throwError (head4 ++ " missing an id in a response")


testClients ::
  RandomGen g
  => g -> SAMPInfo -> HubTest IO g
testClients gen si = do

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

  -- could come up with a random id that we know is not
  -- valid, but not worth it (and can not guarantee some other
  -- client doesn't register it)
  checkFail "getMetadata with an invalid ID"
    (getMetadataE cl1 "This ID is not valid")
    
  checkFail "getSubscriptions with invalid ID"
    (getSubscriptionsE cl1 "This ID is not valid")

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
  check <- atomicallyIO (countResponseSTM rmap2)
  when (check > necho `div` 2)
    (putLn ("WARNING: looks like hub call/notify not completing " ++
            "quickly (" ++ show check ++ "/" ++ show necho ++ ")"))
  
  let spinWait = do
        c2 <- countResponseSTM rmap2
        c4 <- countResponseSTM rmap4
        unless (c2 == necho && c2 == c4) retry -- (waitMillis 100 >> spinWait)

  atomicallyIO spinWait
        
  let findId rsp = findKey msgIdQueryKey (getSAMPResponseExtra rsp)

  lift (validateClient2 findId clId1 rmap2 msgs tags msgIds2)
  lift (validateClient4 findId clId1 rmap4 sameTag msgIds4 necho)
  
  nrsp2 <- atomicallyIO (countResponseSTM rmap2)
  assert "No more responses for client 2" 0 nrsp2

  nrsp4 <- atomicallyIO (countResponseSTM rmap4)
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
  putLn "Finished closing down client 4"

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
  forM_ [0..3::Int] $ \ie -> do  -- TODO: use a max of 3 for testing for now
                                 -- as larger values take too long
    let num = 10 ^ ie
        xs = toSValue (map toSValue [1 .. num::Int])
        pars = M.singleton "list" xs
        msg = toSAMPMessageMT echoMTYPE pars M.empty

    putLn ("call and wait with list size " ++ show num)
    rsp <- lift (callAndWaitE cl2 clId1 msg Nothing)
    -- TODO: hsamp-hub does not time out correctly for long messages 
    
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

  gen4 <- lift (allTests gen3 cl3 rmap3 otherIds)

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
  putLn "Finished closing down client 3"
  
  assertTestClients cl1 [clId2]
  assertTestClients cl2 [clId1]
  
  -- unregister them in a different order to those are created
  removeClient cl1 tid1
  putLn "Finished closing down client 1"
  
  removeClient cl2 tid2
  putLn "Finished closing down client 1"

  -- check that unregistering an unregistered client is an error
  checkFail "Client already unregistered" (unregisterE cl1)
  
  return gen4

