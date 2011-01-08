
ghc -hide-package monads-fd --make -o basicclient -Wall BasicClient.lhs

> module Main where
>
> import Data.Maybe (fromJust)
> import Network.SAMP.Standard
>
> -- unsafe conversion routine
> tRS :: String -> RString
> tRS = fromJust . toRString
>
> main :: IO ()
> main = runE $ do
>
>     -- Read information from lockfile to locate and register with hub.
>     conn <- getHubInfoE >>= registerClientE 
>
>     -- Store metadata in hub for use by other applications.
>     vkey <- stringToKeyValE "dummy.version" "0.1-3"
>     md <- toMetadataE "dummy" (Just "Test Application")
>                       Nothing Nothing Nothing
>     declareMetadataE conn (vkey : md)
>     
>     -- Send a message requesting file load to all other
>     -- registered clients, not wanting any response.
>     mt <- toMTypeE "file.load"
>     mparam <- stringToKeyValE "filename" "/tmp/foo.bar"
>     msg <- toSAMPMessage mt [mparam]
>     _ <- notifyAllE conn msg
>
>     -- Unregister
>     unregisterE conn
