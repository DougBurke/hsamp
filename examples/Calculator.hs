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

-- Support clients that act as a simple calculator
--
-- Note that there is NO support for handling over- or underflow,
-- but as the expeced value is calculated using the same
-- system it should at least return consistent answers
-- (and for the tests the values are bounded so that overflow
-- or underflow will not happen).
--

-- the store/checks are being written by converting the Java version
-- from the middle outwards; that is, I am converting things hap-hazardly
-- which is going to lead to confusion and wasted effort.

module Calculator (
  runCalcStorm
  ) where

import qualified Data.Map.Strict as M

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Monad (forM, forM_, replicateM, unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Random (RandT, runRandT, uniform)
import Control.Monad.Random.Class (MonadRandom(..))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)

import Data.Maybe (fromJust, isJust)
import Data.List (transpose, unzip4, zip4)

import Network.SAMP.Standard.Client ( callAndWaitE
                                    , declareMetadataE
                                    , declareSubscriptionsSimpleE
                                    , notifyE
                                    , toMetadata
                                    , unregisterE
                                    )
import Network.SAMP.Standard.Server ( SAMPCallFunc
                                    , SAMPCallMap
                                    , SAMPNotificationFunc
                                    , SAMPNotificationMap
                                    , SAMPResponseFunc
                                    , callE
                                    , setXmlrpcCallbackE
                                    , simpleClientServer
                                    )
import Network.SAMP.Standard.Server.Scotty (runServerSignal)
import Network.SAMP.Standard.Types ( ClientName
                                   , Err
                                   , MessageTag
                                   , MType
                                   , SAMPConnection(scId)
                                   , SAMPInfo
                                   , SAMPMessage
                                   , SAMPResponse
                                   , getKey
                                   , getSAMPMessageParams
                                   , getSAMPMessageType
                                   , getSAMPResponseResult
                                   , handleError
                                   , isSAMPSuccess
                                   , runE
                                   , toMessageTag
                                   , toSAMPMessageMT
                                   , toRString
                                   , toRStringE
                                   , toSAMPResponse
                                   , toSAMPResponseError
                                   , toSValue
                                   )

import System.Random (RandomGen)

import TestUtils (HubTest
                 , EvalCounter
                 , SendCounter
                 , Store
                 , assert
                 , makeClients
                 , newEvalCounter
                 , newSendCounter
                 , increaseCounter
                 , getCounter
                 , addStore
                 , getStore
                 , newStore
                 , nullStore
                 , mapConcurrentlyE
                 , putLn)
import Utils (getAddress, getSocket, waitMillis, waitForProcessing)

type CalcStore = Store MessageTag Int

-- Combine random and error monads
type RandErr g m a = RandT g (ExceptT String m) a

{-
toMaybeE :: Err Maybe a -> Maybe a
toMaybeE = handleError (const Nothing)
-}

toEitherE :: Err (Either String) a -> Either String a
toEitherE = handleError Left

addMTYPE, subMTYPE, mulMTYPE, divMTYPE :: MType
addMTYPE = "calc.int.add"
subMTYPE = "calc.int.sub"
mulMTYPE = "calc.int.mul"
divMTYPE = "calc.int.div"

allMTYPES :: [MType]
allMTYPES = [addMTYPE, subMTYPE, mulMTYPE, divMTYPE]

-- | Starts up the server in a new thread, waiting until it
--   has been started before returning.
--
startServer ::
  SAMPConnection
  -> Err IO (String, EvalCounter, CalcStore, ThreadId)
  -- ^ The URL, counter, store, and thread Id of the server.
  --   The counter represents the number of messages processed
  --   (i.e. received) by the server.
startServer conn = do
  mvar <- liftIO newEmptyMVar
  ctr <- liftIO newEvalCounter
  store <- liftIO newStore 
  sock <- liftIO getSocket
  fullUrl <- liftIO (getAddress sock)

  toRStringE fullUrl >>= setXmlrpcCallbackE conn

  let baseUrl = reverse (dropWhile (/='/') (reverse fullUrl))
      server = do
        let act = simpleClientServer conn
                  (notifications ctr)
                  (calls ctr)
                  (rfunc store)
        runServerSignal sock baseUrl act mvar
      
  tid <- liftIO (forkIO server)
  liftIO (takeMVar mvar)
  return (fullUrl, ctr, store, tid)


-- samp.client.receiveNotification
--
notifications :: EvalCounter -> SAMPNotificationMap
notifications ctr = [ ("*", handleNotify ctr) ]

-- samp.client.receiveCall
--
calls :: EvalCounter -> SAMPCallMap
calls ctr = [("*", handleCall ctr)]

-- samp.client.receiveResponse
--
-- validate that the response is correct
rfunc :: CalcStore -> SAMPResponseFunc
rfunc store _ clName msgTag rsp = do
  mExp <- getStore msgTag store
  unless (isJust mExp)
    (error ("No stored value for tag=" ++ show msgTag ++
            " client=" ++ show clName))

  runE (validateResponse ("rfunc tag=" ++ show msgTag) rsp (fromJust mExp))
    

handleNotify :: EvalCounter -> SAMPNotificationFunc
handleNotify ctr _ _ msg = do
  mrsp <- doCalc ctr msg
  case mrsp of
    Right _ -> return ()
    Left emsg -> error emsg


handleCall :: EvalCounter -> SAMPCallFunc
handleCall ctr _ _ _ msg = do
  mx <- doCalc ctr msg
  let mtype = getSAMPMessageType msg
      -- TODO: need a better solution (i.e. should be in Types)
      mtypeR = fromJust (toRString (show mtype))

  case mx of
    Right x -> let pars = M.singleton "x" (toSValue x)
               in return (toSAMPResponse pars M.empty)
    Left emsg ->
      let rmsg = case toRString emsg of
            Just v -> v
            _ -> "Internal Error: failed call " ++ mtypeR
      in return (toSAMPResponseError rmsg M.empty M.empty)


getOp :: Monad m => MType -> Err m (Int -> Int -> Int)
getOp mt | mt == addMTYPE = return (+)
         | mt == subMTYPE = return (-)
         | mt == mulMTYPE = return (*)
         | mt == divMTYPE = return div
         | otherwise      = throwError ("Unrecognized MTYPE: " ++ show mt)

                            
doCalc :: EvalCounter -> SAMPMessage -> IO (Either String Int)
doCalc ctr msg = do
  let params = getSAMPMessageParams msg

      act = do
        op <- getOp (getSAMPMessageType msg)
        a <- getKey "a" params
        b <- getKey "b" params
        return (a `op` b)
      mres = handleError Left act

  case mres of
    Right ans -> do
      increaseCounter ctr
      return (Right ans)
    Left emsg -> return (Left emsg)
    

-- | Create a randomly-generated message.
--
--   This could just be Rand (SAMPMessage, Int) but easiest to
--   make this easier.
createMessage ::
  (RandomGen g, Monad m)
  => RandErr g m (SAMPMessage, Int)
  -- ^ The message and the expected answer.
createMessage = do
  mtype <- uniform allMTYPES
  op <- lift (getOp mtype)
  a <- getRandomR (0, 999)
  b <- (500 +) <$> getRandomR (0, 499)
  let params = M.fromList [("a", toSValue a), ("b", toSValue b)]
      ans = a `op` b
  return (toSAMPMessageMT mtype params M.empty, ans)


validateResponse ::
  Monad m => String -> SAMPResponse -> Int -> Err m ()
validateResponse lbl rsp expVal = do
  let params = fromJust (getSAMPResponseResult rsp)
      
  unless (isSAMPSuccess rsp)
    (throwError (lbl ++ " response is not a success"))

  case toEitherE (getKey "x" params) of
    Right x -> unless (x == expVal)
               (throwError (lbl ++ " expected x=" ++
                            show expVal ++ " got x=" ++ show x))

    Left msg -> throwError msg


-- | Send a message using a randomly-selected method:
--      notify, synchronous, asynchronous
--
type SendMethod =
  SAMPConnection
  -> ClientName
  -> SAMPMessage
  -> Int
  -- ^ The expected answer
  -> Err IO ()


sendNotify :: SendMethod
sendNotify conn receiver msg _ = notifyE conn receiver msg

sendSync :: SendMethod
sendSync conn receiver msg expVal = do
  rsp <- callAndWaitE conn receiver msg Nothing
  validateResponse ("sendSync: receiver=" ++ show receiver) rsp expVal

    
-- | Convert a "counter" value to  a "unique" message tag
--
getTag :: Int -> MessageTag
getTag t =
  let tval = fromJust (toRString (show t))
  in toMessageTag ("tag-" ++ tval)


sendAsync ::
  CalcStore
  -> Int -- counter
  -> SendMethod
sendAsync store uniqTag conn receiver msg expVal = do
  let tag = getTag uniqTag
  void (callE conn receiver tag msg)
  liftIO (addStore tag expVal store)


{-
TODO: if there is an error, it's not going to necessarily
  kill the test. Either need to get the calculator threads
  to kill the parent thread, or to send some form of an
  error message back.

-}

runCalcStorm ::
  RandomGen g
  => String
  -- ^ representation of the random number generator used to seed the
  --   whole test (not the next argument); this is used to provide
  --   primitive logging to allow (hopefully) regenerating
  --   problematic test cases
  --   [could be output earlier/not sent here but screen output may
  --    be too long]
  -> g
  -> SAMPInfo
  -> Int     -- ^ number of clients
  -> Int     -- ^ number of queries
  -> HubTest IO g
runCalcStorm genStr ogen si nclient nquery = do

  let ntotal = nclient * nquery
  
  putLn ("--> creating " ++ show nclient ++ " calculator clients")
  clinfos <- makeCalculators si nclient
  putLn "--> created calculator clients"

  let (conns, evalCounters, stores, tids) = unzip4 clinfos
      clNames = map scId conns

  sendCounters <- replicateM nclient (liftIO newSendCounter)
  let sendMap = M.fromList (zip clNames sendCounters)

  -- create the random messages, for each client
  let mkStorm = forM clinfos (createStorm nquery sendMap)
  (msgInfos, ngen) <- lift (runRandT mkStorm ogen)

  -- "mix up" the messages, otherwise the first nquery
  -- ones in the list are from the same client.
  --
  let msgs = concat (transpose msgInfos)
  assert "[programmer check] list lengths match"
    (length (concat msgInfos)) (length msgs)
  
  -- send them concurrently
  void (lift (mapConcurrentlyE id msgs))

  putLn ("Initial RNG: " ++ genStr)
  putLn "Waiting for calc storm clients to finish ..."
  
  -- wait until all the messages have been sent.
  let waitSum counters =
        let go = do
              cts <- forM counters getCounter
              let ncts = sum cts
              when (ncts < ntotal) (waitMillis 100 >> go)
        in go

      -- wait for the call async messages to be processed
      waitStore store =
        let go = do
              isEmpty <- nullStore store
              unless isEmpty (waitMillis 100 >> go)
        in go

  putLn "... waiting for sendCounters"
  liftIO (waitSum sendCounters)
  putLn "... waiting for evalCounters"
  liftIO (waitSum evalCounters)
  putLn "... waitStore"
  liftIO (forM_ stores waitStore)

  -- Now the calls have been made, can find out the number of
  -- messages sent to each client.
  sentCounts <- forM sendCounters (liftIO . getCounter)

  putLn "Closing down storm clients ..."
  finishStorm (zip conns tids)

  -- Final check for the number of sent and received messages.
  --
  putLn "Final storm checks..."

  forM_ (zip4 sentCounts sendCounters evalCounters clNames) $
    \(nexp, sendCtr, evalCtr, name) -> do
      nsent <- liftIO (getCounter sendCtr)
      neval <- liftIO (getCounter evalCtr)

      -- this should not happen, but check anyway
      assert ("Unexpected call count for " ++ show name) nexp nsent

      -- check that nothing else has happened since the calls were
      -- processed above.
      assert ("Unexpected eval count for " ++ show name) nsent neval
        
  return ngen


-- | Create the information needed to create nquery calls
--   from this client. Randomised:
--      - message content (operation and arguments)
--      - receiver
--      - method for sending the message
--
--   TODO: this need not be in IO, since do not need to increase
--   the SendCounter, as could return the names of the clients being
--   sent a message (so can calculate it outside this routine).
--   This is a hold-over from an earlier method and should be
--   cleaned up (once got it working).
--
createStorm ::
  RandomGen g
  => Int
  -- ^ number of queries
  -> M.Map ClientName SendCounter
  -- ^ the storm clients (including this one); the SendCounter
  --   of the messages being sent a message will be increased
  --   here (not necessary now sending back the client name
  --   that is selected, but leave in for now)
  -> (SAMPConnection, EvalCounter, CalcStore, ThreadId)
  -- ^ the client making the call.
  -> RandErr g IO [Err IO ()]
  -- ^ Returns the action to make the call
createStorm nquery nameMap (conn, _, store, _) = do

  -- the assumption is that others is not empty
  let others = M.toList (M.delete (scId conn) nameMap)

      ctrs = [1..nquery]
      act ctr = do
        (msg, expVal) <- createMessage
        (receiver, sendCounter) <- uniform others
        sendMethod <- uniform [sendNotify, sendSync, sendAsync store ctr]
        liftIO (increaseCounter sendCounter)
        return (sendMethod conn receiver msg expVal)

  forM ctrs act


-- | Create the requested number of calculator clients,
--   asynchronously.
makeCalculators ::
  SAMPInfo
  -> Int
  -> HubTest IO [(SAMPConnection, EvalCounter, CalcStore, ThreadId)]
makeCalculators si nclient = do

  conns <- makeClients si nclient

  let name = "Calculator"
      desc = "Rudimentary integer arithmetic application"
      mds = toMetadata name (Just desc) Nothing Nothing Nothing

      addMetadata conn = declareMetadataE conn mds
      subscribe conn = declareSubscriptionsSimpleE conn allMTYPES

  -- asynchronous metadata calls
  void (lift (mapConcurrentlyE addMetadata conns))

  -- start the servers synchronously, since this is not part of
  -- the hub
  res <- forM conns (lift . startServer)
  let (_, ctrs, stores, tids) = unzip4 res

  -- remember, have to declate the subscriptions *after*
  -- making the client callable!
  void (lift (mapConcurrentlyE subscribe conns))

  return (zip4 conns ctrs stores tids)

  
-- | Unregister the connections and kill the associated servers,
--   with the SAMP calls made asynchronously.
--
finishStorm :: [(SAMPConnection, ThreadId)] -> HubTest IO ()
finishStorm args = do
  let (conns, tids) = unzip args
  putLn ("Removing " ++ show (length args) ++ " clients")
  void (lift (mapConcurrentlyE unregisterE conns))
  forM_ tids (liftIO . killThread)
  putLn "Done."
  waitForProcessing 100

