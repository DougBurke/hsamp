> {-# LANGUAGE OverloadedStrings #-}

Utility routines

> module Utils
>        (
>         doE,
>         createClient, authorMetadata,
>         Barrier, newBarrier, syncPrint,
>         displayKV, showKV,
>         getKeyStr,
>        ) where

> import System.Exit (exitFailure)
> import System.IO

> import Control.Concurrent.MVar
> import Control.Concurrent (forkIO)

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

> type Barrier = MVar ()

> newBarrier :: IO Barrier
> newBarrier = newMVar ()

Displays the given string to stdout, ensuring that it is done as an
atomic operation (as long as other displays are also done with
syncPrint using the same barrier). The print occurs in a separate
thread so that if there is a delay due to the barrier being used
elsewhere it won't affect the calling routine.

TODO: change to send to a channel, with a forked process printing
   the arrays sent to the channel

> syncPrint :: Barrier -> [String] -> IO ()
> syncPrint barrier msg = forkIO (withMVar barrier $ \_ -> putStr (unlines msg)) >> return ()

Try and ensure sequential output to the screen. Given that we want to respond
to the hub and display a message it may be better to use a Channel to send
the screen output to a separate display thread (to try and avoid any waiting for the
barrier)?

> displayKV :: SAMPKeyValue -> String
> displayKV (k,v) = "  " ++ fromRString k ++ " -> " ++ showSAMPValue v

> showKV :: SAMPKeyValue -> IO ()
> showKV = putStrLn . displayKV

> getKeyStr :: [SAMPKeyValue] -> RString -> Maybe RString
> getKeyStr kvs key =
>     case lookup key kvs of
>         Just (SAMPString s) -> Just s
>         _ -> Nothing

