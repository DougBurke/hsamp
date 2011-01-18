> {-# LANGUAGE OverloadedStrings #-}

Utility routines

> module Utils
>        (
>         doE,
>         createClient, authorMetadata,
>         PrintChannel, newPrintChannel, runPrintChannel, startPrintChannel, syncPrint,
>         displayKV, showKV,
>         getKeyStr,
>        ) where

> import System.Exit (exitFailure)
> import System.IO

> import Control.Concurrent.Chan
> import Control.Concurrent (ThreadId, forkIO)

> import Network.SAMP.Standard.Types
> import Network.SAMP.Standard.Client

Run a set of SAMP commands and exit on error.

> doE :: Err IO a -> IO a
> doE = handleError
>         (\emsg -> hPutStrLn stderr ("ERROR: " ++ emsg) >> exitFailure)

Set up a simple client (i.e. with limited metadata)

> createClient :: String -> String -> Err IO SAMPConnection
> createClient name description =
>      getHubInfoE >>=
>      registerClientE >>= \conn ->
>      toMetadataE name (Just description)
>          Nothing Nothing Nothing >>= \md ->
>      declareMetadataE conn (md ++ authorMetadata) >>
>      return conn

> authorMetadata :: [SAMPKeyValue]
> authorMetadata = 
>     [("author.name", "Doug Burke"),
>      ("author.affiliation", "Smithsonian Astrophysical Observatory"),
>      ("author.email", "dburke@cfa.harvard.edu")]

> type PrintChannel = Chan [String]

Returns the print channel and starts the print thread

> startPrintChannel :: IO (PrintChannel, ThreadId)
> startPrintChannel = do
>     pchan <- newPrintChannel
>     pid <- runPrintChannel pchan
>     return (pchan, pid)

> newPrintChannel :: IO PrintChannel
> newPrintChannel = newChan

Starts a new thread which prints all the text sent to
the print channel. The return value is the id of this
thread.

> runPrintChannel :: PrintChannel -> IO ThreadId
> runPrintChannel pchan = 
>     let doit = readChan pchan >>= putStr . unlines >> doit
>     in forkIO doit

Displays the given string to stdout, ensuring that it is done as an
atomic operation (as long as other displays are also done with
syncPrint using the same print channel). The print occurs in a separate
thread.

> syncPrint :: PrintChannel -> [String] -> IO ()
> syncPrint = writeChan

> displayKV :: SAMPKeyValue -> String
> displayKV (k,v) = "  " ++ fromRString k ++ " -> " ++ showSAMPValue v

> showKV :: SAMPKeyValue -> IO ()
> showKV = putStrLn . displayKV

> getKeyStr :: [SAMPKeyValue] -> RString -> Maybe RString
> getKeyStr kvs key =
>     case lookup key kvs of
>         Just (SAMPString s) -> Just s
>         _ -> Nothing

