> {-# LANGUAGE OverloadedStrings #-}
>
> module Main where
>
> import Network.SAMP.Standard
>
> -- We rely here on the OverloadedStrings extension to
> -- convert from Strings to the required types, but explicit
> -- conversions are also provided by routines like
> -- @toMTypeE@ and @stringToKeyValE@.
> --
>
> main :: IO ()
> main = withSAMP $ runE $ do
>
>     -- Read information from lockfile to locate and register with hub.
>     conn <- getHubInfoE >>= registerClientE 
>
>     -- Store metadata in hub for use by other applications.
>     let vkey = ("dummy.version", "0.1-3")
>     md <- toMetadataE "dummy" (Just "Test Application")
>                       Nothing Nothing Nothing
>     declareMetadataE conn (vkey : md)
>     
>     -- Send a message requesting file load to all other
>     -- registered clients, not wanting any response.
>     msg <- toSAMPMessage "file.load" [("filename", "/tmp/foo.bar")]
>     _ <- notifyAllE conn msg
>
>     -- Unregister
>     unregisterE conn
