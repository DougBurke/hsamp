
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
> -- Report on clients that are subscribed to table.load.votable message.
> --
> -- Note that here we explicitly convert to a @MType@ using @toMTypeE@
> -- rather than relying on the OverloadedStrings extension.
>
> act :: SAMPConnection -> Err IO ()
> act conn = do
>     msg <- toMTypeE "table.load.votable"
>     clients <- getSubscribedClientsE conn msg
>     forM_ clients $ \(cl,_) -> do
>         liftIO $ putStrLn $ "Client: " ++ fromRString cl
>         name <- getClientNameE conn cl
>         case name of
>             Just n -> liftIO $ putStrLn $ "   aka: " ++ fromRString n
>             _ -> return ()
