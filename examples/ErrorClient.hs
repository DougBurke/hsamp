{-|
------------------------------------------------------------------------
Copyright   :  (c) Douglas Burke 2011, 2013, 2015
License     :  BSD3

Maintainer  :  dburke.gw@gmail.com
Stability   :  unstable
Portability :  requires haxr
------------------------------------------------------------------------
-}

module Main where

import qualified Control.Exception as CE

import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)

import Network.SAMP.Standard 

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO
import System.Log.Logger

usage :: IO ()
usage = do
  name <- getProgName
  hPutStrLn stderr ("Usage: " ++ name ++ " [--debug] mtype1 .. mtypeN")

-- | Strip out the --debug flag if it exists. There is no validation of
--   the arguments.
procArgs :: 
  [String]  -- ^ command-line arguments
  -> (Bool, [String]) -- ^ flag is @True@ if --debug is given
procArgs = foldr go (False, [])
  where
    go x (f, xs) | x == "--debug" = (True, xs)
                 | otherwise      = (f, x:xs)

main :: IO ()
main = do
    (debug, args) <- procArgs `fmap` getArgs
    when debug $ updateGlobalLogger "SAMP" (setLevel DEBUG)
    if null args 
      then usage >> exitFailure
      else do
        -- the conversion routines like @toMTypeE@ are safe to run
        -- outside of @withSAMP@.
        mtypes <- runE (mapM toMTypeE args)
        withSAMP $ doSAMP mtypes

doSAMP :: [MType] -> IO ()
doSAMP mtypes = do
    -- register the client
    conn <- runE $ getHubInfoE >>= registerClientE
    putStrLn "Registered with the SAMP hub."

    -- catch a user interrupt so that we can unregister the client
    let hdlr :: CE.AsyncException -> IO a
        hdlr CE.UserInterrupt = runE (unregisterE conn) >> CE.throwIO CE.UserInterrupt
        hdlr e = CE.throwIO e

    -- Awkward error handling to make sure we unregister on an error. It is
    -- not quite correct (need to say why, as I can't remember what I meant
    -- by this)
    handleError (\m -> runE (unregisterE conn) >> fail m)
                (act conn mtypes) `CE.catch` hdlr
    runE $ unregisterE conn
    putStrLn "Unregistered client."

-- Report on clients that are subscribed to the given message.

report :: SAMPConnection -> MType -> Err IO ()
report conn msg = do
    liftIO $ putStrLn $ "Clients subscribed to " ++ show msg
    clients <- getSubscribedClientsE conn msg
    forM_ clients $ \(cl,_) -> do
        liftIO $ putStr $ "  " ++ show cl
        name <- getClientNameE conn cl
        case name of
            Just n -> liftIO $ putStr $ " -> " ++ show n
            _ -> return ()
        liftIO $ putStr "\n"

act :: SAMPConnection -> [MType] -> Err IO ()
act conn = mapM_ (report conn)
