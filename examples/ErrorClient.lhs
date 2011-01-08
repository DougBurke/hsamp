
ghc -hide-package monads-fd --make -o errorclient -Wall ErrorClient.lhs

> module Main where
>
> import qualified Control.Exception as CE
> import Control.Monad (forM_)
> import Control.Monad.Trans (liftIO)
> import Data.Maybe (fromJust)
>
> import Network.SAMP.Standard 
>
> main :: IO ()
> main = do
>     -- register the client
>     conn <- runE $ getHubInfoE >>= registerClientE
>     putStrLn "Registered with the SAMP hub"
>
>     -- catch a user interrupt so that we can unregister the client
>     let hdlr :: CE.AsyncException -> IO a
>         hdlr CE.UserInterrupt = runE (unregisterE conn) >> CE.throwIO CE.UserInterrupt
>         hdlr e = CE.throwIO e
>
>     -- Awkward error handling to make sure we unregister on an error. It is
>     -- not quite correct.
>     handleError (\m -> runE (unregisterE conn) >> fail m) (act conn) `CE.catch` hdlr
>     runE $ unregisterE conn
>     putStrLn "Unregistered client"
>
> sName :: RString
> sName = fromJust $ toRString "samp.name"
>
> -- report on clients that are subscribed to table.load.votable message
> act :: SAMPConnection -> Err IO ()
> act conn = do
>     msg <- toMTypeE "table.load.votable"
>     clients <- getSubscribedClientsE conn msg
>     forM_ clients $ \(cl,_) -> do
>         liftIO $ putStrLn $ "Client: " ++ show cl
>         md <- getMetadataE conn cl
>         -- this will error out if the @samp.name@ field is not set;
>         -- so we really should handle this case
>         name <- getKey sName md :: Err IO RString
>         liftIO $ putStrLn $ "   aka: " ++ show name
