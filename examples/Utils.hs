{-# LANGUAGE OverloadedStrings #-}

-- Utility routines

module Utils
       (
        doE,
        createClient, authorMetadata,
        PrintChannel, newPrintChannel, runPrintChannel, startPrintChannel, syncPrint,
        displayKV, showKV,
        getKeyStr,
        getSocket, getAddress, hostName, hostNameBS,
        waitMillis, waitForProcessing
       ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Network as N

import Control.Concurrent.Chan
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)

import Network.SAMP.Standard.Types
import Network.SAMP.Standard.Client
import Network.SAMP.Standard.Setup
import Network.Socket (Socket, PortNumber, socketPort)

import System.Exit (exitFailure)
import System.IO

-- | Run a set of SAMP commands and exit on error.

doE :: Err IO a -> IO a
doE = handleError
        (\emsg -> hPutStrLn stderr ("ERROR: " ++ emsg) >> exitFailure)

-- | Set up a simple client (i.e. with limited metadata)

createClient :: String -> String -> Err IO SAMPConnection
createClient name description =
    sampLockFileE >>=
    getHubInfoE >>=
    registerClientE >>= \conn ->
    toMetadataE name (Just description)
                Nothing Nothing Nothing >>= \md ->
    declareMetadataE conn (md `M.union` authorMetadata) >>
    return conn

authorMetadata :: SAMPMapValue
authorMetadata =
  M.fromList
    [("author.name", "Doug Burke"),
     ("author.affiliation", "Smithsonian Astrophysical Observatory"),
     ("author.email", "dburke.gw@gmail.com")]

type PrintChannel = Chan [String]

-- | Returns the print channel and starts the print thread

startPrintChannel :: IO (PrintChannel, ThreadId)
startPrintChannel = do
    pchan <- newPrintChannel
    pid <- runPrintChannel pchan
    return (pchan, pid)

newPrintChannel :: IO PrintChannel
newPrintChannel = newChan

-- | Starts a new thread which prints all the text sent to
--   the print channel. The return value is the id of this
--   thread.

runPrintChannel :: PrintChannel -> IO ThreadId
runPrintChannel pchan = 
    let doit = readChan pchan >>= putStr . unlines >> doit
    in forkIO doit

-- | Displays the given string to stdout, ensuring that it is done as
--   an atomic operation (as long as other displays are also done with
--   syncPrint using the same print channel). The print occurs in a
--   separate thread.

syncPrint :: PrintChannel -> [String] -> IO ()
syncPrint = writeChan

displayKV :: SAMPMapElement -> String
displayKV (k,v) = "  " ++ fromRString k ++ " -> " ++ showSAMPValue v

showKV :: SAMPMapElement -> IO ()
showKV = putStrLn . displayKV

getKeyStr :: SAMPMapValue -> RString -> Maybe RString
getKeyStr kvs key =
    case M.lookup key kvs of
        Just (SAMPString s) -> Just s
        _ -> Nothing

-- | Return a socket with a randomly-chosen port for use by the server.

getSocket :: IO Socket
getSocket = N.listenOn (N.PortNumber (fromIntegral (0::Int)))

getAddress :: Socket -> IO String
getAddress = fmap hostName . socketPort

-- | The location of the server we create to handle incoming messages
--   from the SAMP hub. Note that this includes the XML-RPC route
--   since in this case we do not support any other access.

hostName :: PortNumber -> String
hostName portNum = "http://127.0.0.1:" ++ show portNum ++ "/xmlrpc"

hostNameBS :: PortNumber -> B.ByteString
hostNameBS = B.pack . hostName

waitMillis ::
  Int  -- ^ delay in milliseconds
  -> IO ()
waitMillis n = threadDelay (n * 1000)

-- Pause processing for a small amount of time to try and let
-- threads process everything.
--
waitForProcessing ::
  MonadIO m
  => Int  -- ^ delay in milliseconds (as that's what HubTester uses)
  -> m ()
waitForProcessing = liftIO . waitMillis


