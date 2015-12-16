{-# LANGUAGE OverloadedStrings #-}

{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2011, 2013, 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------
-}

{-
Send a SAMP message (with potential arguments) to all interested clients.

Usage:

   ./sender [sync|async|notify] mtype [param1=value1] [param2=value2] ... [paramN=valueN]

            --debug

TODO:

  - target name(s) as well as ids

  - sendername/metadata

  - improved error handling

  - better checks of responses

  - better command-line handling

  - allow sending of "extra" values
-}

module Main (main) where

import qualified Control.Exception as CE
import qualified Network as N

import Control.Concurrent
import Control.Concurrent.ParallelIO.Global
import Control.Monad (forM_, unless, when)

import Data.Maybe (isNothing)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)

import Network.SAMP.Standard
import Network.SAMP.Standard.Server.Scotty
import Network.Socket (Socket)

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO
import System.IO.Error
import System.Log.Logger
import System.Random
import System.Timeout (timeout)
    
import Utils

-- convert seconds to milliseconds, error-ing out if the converted
-- value would cause overflow.
sToMS :: Int -> Int
sToMS n =
    let nMax = maxBound `div` scale
        scale = 1000000
    in if n <= nMax then scale * n else error "Timeout too long"
       
timeoutPeriod :: Int
timeoutPeriod = sToMS 10

usage :: IO ()
usage = do
  name <- getProgName
  hPutStrLn stderr $ "Usage: " ++ name ++ " --debug [sync|async|notify] mtype [param1=value1] .. [paranN=valueN]"
  exitFailure

cLogger :: String
cLogger = "SAMP.Application"

-- Start messages with %%% to distinguish them from library messages
dbg :: String -> IO ()
dbg = debugM cLogger . ("%%% " ++)

data ConnMode = Sync | ASync | Notify deriving (Eq, Show)

getConnection :: [String] -> (ConnMode, [String])
getConnection ("sync":xs) = (Sync, xs)
getConnection ("async":xs) = (ASync, xs)
getConnection ("notify":xs) = (Notify, xs)
getConnection xs = (Sync, xs)

getMType :: [String] -> Either String (MType, [String])
getMType [] = Left "Missing required mtype argument"
getMType (name:xs) = 
    handleError Left $ fmap (flip (,) xs) (toMTypeE name)

getKV :: String -> Either String SAMPKeyValue
getKV arg = do
    let (l,r) = break (=='=') arg
    lval <- if null l
              then Left $ "Expected key=value but sent '" ++ arg ++ "'"
              else Right l
    rval <- if null r || r == "="
              then Left $ "Expected key=value but sent '" ++ arg ++ "'"
              else Right $ tail r
    handleError Left $ stringToKeyValE lval rval

-- | Strip out the --debug flag if it exists. There is no validation of
--   the arguments.
procArgs :: 
  [String]  -- ^ command-line arguments
  -> (Bool, [String]) -- ^ flag is @True@ if --debug is given
procArgs = foldr go (False, [])
  where
    go x (f, xs) | x == "--debug" = (True, xs)
                 | otherwise      = (f, x:xs)

processArgs :: [String] -> Either String ((ConnMode, MType, [SAMPKeyValue]), Bool)
processArgs args = do
  let (dbgFlag, args1) = procArgs args
      (conmode, args2) = getConnection args1
      
  (mtype, args3) <- getMType args2
  kvals <- mapM getKV args3
  return ((conmode, mtype, kvals), dbgFlag)

main :: IO ()
main = do
    args <- getArgs
    case processArgs args of
        Left emsg -> hPutStrLn stderr ("ERROR: " ++ emsg ++ "\n") >> usage
        Right (x, dbgFlag) -> do
            when dbgFlag $ updateGlobalLogger "SAMP" (setLevel DEBUG)
            withSAMP $ do
              conn <- doE $ createClient "hsamp-sender"
                              "Send a message to interested clients."

              -- The assumption is that we only need to unregister if we get
              -- a user interrupt but not for other errors. This is actually not
              -- correct since we use runE/fail within processMessage for situations
              -- where we need to clean up. So also look for user exceptions.

              let asyncHdlr :: CE.AsyncException -> IO ()
                  asyncHdlr e = when (e == CE.UserInterrupt) (doE (unregisterE conn)) >>
                                  hPutStrLn stderr ("ERROR: " ++ show e) >> exitFailure
    
                  ioHdlr :: CE.IOException -> IO ()
                  ioHdlr e | isUserError e = doE (unregisterE conn) >>
                                               hPutStrLn stderr ("ERROR: " ++ ioeGetErrorString e) >> exitFailure
                           | otherwise     = hPutStrLn stderr ("ERROR: " ++ show e) >> exitFailure

                  otherHdlr :: CE.SomeException -> IO ()
                  otherHdlr e = hPutStrLn stderr ("ERROR: " ++ show e) >> exitFailure

              processMessage conn x `CE.catches`
                  [CE.Handler ioHdlr, CE.Handler asyncHdlr, CE.Handler otherHdlr]
              doE (unregisterE conn)
              exitSuccess

{- 

There is a chance that targets may be added or removed in between the
getSubscribedClientsE call and the later processing, but we except
this for now (for the synchronous case we need to use the
getSubscribedClientsE route, but not for the other two methods which
could skip this step).

-}

processMessage ::
    SAMPConnection
    -> (ConnMode, MType, [SAMPKeyValue])
    -> IO ()
processMessage conn (cmode, mtype, params) = do
  targets <- runE (getSubscribedClientsE conn mtype)
  if null targets
    then putStrLn $ "No clients are subscribed to the message " ++ show mtype
    else do
      msg <- runE (toSAMPMessage mtype params [])
      (pchan, _) <- startPrintChannel -- not needed for notification case but do anyway
      case cmode of
        Sync   -> parallel_ (map (sendSync pchan conn msg . fst) targets)
                  >> stopGlobalPool

        Notify -> do
          putStrLn "Notifications sent to:" 
          tgts <- runE (notifyAllE conn msg
                        >>= mapM (\n -> fmap ((,) n) (getClientNameE conn n)))
          forM_ tgts $ \t -> putStrLn ("    " ++ showTarget t)

        ASync  -> do
          chan <- newChannel
          tvar <- emptyTimeVar
          
          sock <- getSocket
          _ <- makeServer pchan tvar chan conn sock
          dbg "Created server for async"
          
          clients <- sendASync tvar conn msg

          cv <- newMVar clients
          flag <- timeout timeoutPeriod (waitForCalls pchan chan cv)
          case flag of
            Just _ -> putStrLn "Received all calls"
            _ -> putStrLn "Timed out waiting for calls"
            
          -- do we need to do this? should this be done within a resource
          -- "handler" (to make sure the socket is closed on error)?
          dbg "Closing socket"
          N.sClose sock

          unless (isNothing flag) $ do
            rclients <- takeMVar cv 
            case rclients of
              [] -> return ()
              [cl] -> fail ("The following client failed to respond: " ++
                            show cl)
              _ -> fail ("The following clients failed to respond: " ++
                         unwords (map show rclients))

type TimeVar = MVar UTCTime
type Channel = Chan ClientName

type Target = (ClientName, Maybe RString)
    
emptyTimeVar :: IO TimeVar
emptyTimeVar = newEmptyMVar

newChannel :: IO Channel
newChannel = newChan

printResponse ::
    PrintChannel -> NominalDiffTime -> Target -> SAMPResponse -> IO ()
printResponse pchan delta target rsp
    | isSAMPErrorOnly rsp = syncPrint pchan $ showError delta target rsp
    | isSAMPWarning rsp   = syncPrint pchan $ showWarning delta target rsp
    | otherwise           = syncPrint pchan $ showSuccess delta target rsp

showTarget :: Target -> String
showTarget (tid, Nothing) = show tid
showTarget (tid, Just tname) = show tid ++ " (" ++ fromRString tname ++ ")"

showHeader ::
    String -> NominalDiffTime -> Target -> String
showHeader lbl delta target = 
    lbl ++ " response from " ++ showTarget target ++ " in " ++ show delta ++ " seconds"

showSuccess ::
    NominalDiffTime -> Target -> SAMPResponse -> [String]
showSuccess delta target rsp = 
    let Just svals = getSAMPResponseResult rsp  
    in showHeader "Successful" delta target : map displayKV svals

showError ::
    NominalDiffTime -> Target -> SAMPResponse -> [String]
showError delta target rsp = 
    let Just (emsg, evals) = getSAMPResponseError rsp
    in [showHeader "Error" delta target, "    " ++ fromRString emsg]
       ++ map displayKV evals

showWarning ::
    NominalDiffTime -> Target -> SAMPResponse -> [String]
showWarning delta target rsp = 
    let Just svals = getSAMPResponseResult rsp
        Just (emsg, evals) = getSAMPResponseError rsp
    in [showHeader "Warning" delta target, "    " ++ fromRString emsg]
       ++ if null svals then [] else "  Response" : map displayKV svals
       ++ if null evals then [] else "  Error" : map displayKV evals

sendSync ::
    PrintChannel -> SAMPConnection -> SAMPMessage -> ClientName -> IO ()
sendSync pchan conn msg tid = do
    sTime <- getCurrentTime
    rsp <- runE (callAndWaitE conn tid msg (Just timeoutPeriod))
    eTime <- getCurrentTime
    tname <- handleError (return . const Nothing) $ getClientNameE conn tid
    printResponse pchan (diffUTCTime eTime sTime) (tid,tname) rsp

getMsgTag :: IO MessageTag
getMsgTag = toMessageTag <$> getStdRandom (randomAlphaNumRString 10)

-- TODO: need to handle errors more sensibly than runE here!

sendASync ::
    TimeVar
    -> SAMPConnection
    -> SAMPMessage
    -> IO [ClientName]
sendASync mt conn msg = do
  dbg $ "Sending asynchronous message: " ++ show msg
  sTime <- getCurrentTime
  putMVar mt sTime
  msgTag <- getMsgTag
  map fst <$> runE (callAllE conn msgTag msg)

waitForCalls :: PrintChannel -> Channel -> MVar [ClientName] -> IO ()
waitForCalls pchan chan cv = do
   receiverid <- readChan chan
   modifyMVar_ cv $ \clients ->
     -- TODO: I do not understand the logic of this  
     if receiverid `elem` clients
       then return $ filter (/= receiverid) clients
       else do
         syncPrint pchan ["Ignoring unexpected response from " ++
                          show receiverid]
         return clients
   ncl <- readMVar cv
   unless (null ncl) $ waitForCalls pchan chan cv

{-

TODO: should we respond to the hub shutting down? Almost certainly
(especially for the synchronous case where we currently aren't planning
on setting up the server).

-}

makeServer :: PrintChannel -> TimeVar -> Channel -> SAMPConnection -> Socket -> IO ThreadId
makeServer pchan mt chan conn sock = do
    tid <- myThreadId
    url <- getAddress sock
    urlR <- runE (toRStringE url)

    -- register the messages that we are interested in
    runE $ setXmlrpcCallbackE conn urlR >>
           declareSubscriptionsSimpleE conn []

    -- now run the server
    forkIO $ runServer sock url $ processCall pchan mt chan conn tid

processCall ::
    PrintChannel -> TimeVar -> Channel -> SAMPConnection -> ThreadId -> String -> IO ()
processCall pchan mt chan conn tid =
    simpleClientServer conn (notifications pchan tid) calls (rfunc conn pchan mt chan) 

-- TODO: support hub shutdown

allMT :: MType
allMT = "*"

notifications :: PrintChannel -> ThreadId -> SAMPNotificationMap
notifications pchan _ =
     [(allMT, handleOther pchan)]

calls :: SAMPCallMap
calls = []

handleOther :: PrintChannel -> SAMPNotificationFunc
handleOther pchan _ name msg =
  let mtype = getSAMPMessageType msg
      keys = getSAMPMessageParams msg
      other = getSAMPMessageExtra msg
  in syncPrint pchan $ 
    ("Notification of " ++ show mtype ++ " from " ++ show name) :
    map displayKV keys ++ ["---"] ++ map displayKV other ++ [""]

-- TODO: check that msgid is correct, which means it has to be sent in!

rfunc ::
    SAMPConnection
    -> PrintChannel
    -> TimeVar
    -> Channel
    -> SAMPResponseFunc
rfunc conn pchan mt chan _ clName _ rsp = do
   eTime <- getCurrentTime
   sTime <- readMVar mt
   tname <- handleError (return . const Nothing) $ getClientNameE conn clName
   printResponse pchan (diffUTCTime eTime sTime) (clName,tname) rsp
   writeChan chan clName

