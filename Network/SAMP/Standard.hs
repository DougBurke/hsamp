{-|
Module      :  Network.SAMP.Standard
Copyright   :  (c) Smithsonian Astrophysical Observatory 2011
License     :  BSD-like

Maintainer  :  dburke@cfa.harvard.edu
Stability   :  unstable
Portability :  requires haxr

Support for the Simple Application Messaging Protocol (SAMP) Standard
Profile, described at <http://www.ivoa.net/Documents/latest/SAMP.html>.
This protocol is used in Astronomy to send control information between 
applications, such as
TopCat (<http://www.star.bris.ac.uk/~mbt/topcat/>),
ds9 (<http://hea-www.harvard.edu/RD/ds9/>),
Aladin (<http://aladin.u-strasbg.fr/>)
and the WorldWide Telescope (WWT, <http://www.worldwidetelescope.org/Home.aspx>.

This module almost supports the 1.2 version of the document. Incomplete
features include:

* limited testing.

* Windows support is not tested (and at present does not support using
  the @USERPROFILE@ environment variable for locating the lock file).

* no hub functionality.

-}

module Network.SAMP.Standard (
       module Network.SAMP.Standard.Types,
       module Network.SAMP.Standard.Client,
       module Network.SAMP.Standard.Server

       -- * Types

       -- $types

       -- * Examples

       -- ** Basic client

       -- $basic

       -- ** Simple error handling

       -- $errors

       ) where
       
import Network.SAMP.Standard.Types
import Network.SAMP.Standard.Client
import Network.SAMP.Standard.Server

{- $types

A number of Haskell types are provided to represent the types
used in the SAMP Standard Profile:

  - strings are represented using the 'RString' type (this
    type enforces the character constraints from the profile),
    which is an instance of 'Data.String.IsString' as well
    as providing a number of conversion routines (e.g.
    'toRStringE', 'toRString' and 'fromRString').

  - message names are represented using the 'MType' type,
    which is also an instance of 'Data.String.IsString' and
    has conversion routines like 'toMTypeE', 'toMType'
    and 'fromMType'.

  - SAMP values (string, list and map) are represented using the
    'SAMPValue' type. As a special case, we often represent a
    map just as a list of 'SAMPKeyValue' items rather than include
    the 'SAMPMap' constructor.

  - the 'SAMPConnection' record is used to pass around information needed
    by a client when contacting a hub (it contains the samp secret,
    hub URL, private key for the client as well as the names of the hub
    and the client).

Conversion routines and typeclasses are provided to help convert between the types.
There is limited support for the numeric types (integer, float and bool) defined
in the Standard profile.

Many of the routines are run in the 'Err' monad (which is just the error transformer
with 'String' as the error type). These can be run using 'runE' (which fails on 
error) or 'handleError' (for custom error handling). There is perhaps /too much/
use of this monad in the API.

The prototypical metadata map from the SAMP documentation:

> map metadata = ("samp.name" -> "dummy",
>                 "samp.description.text" -> "Test Application",
>                 "dummy.version" -> "0.1-3");

can be represented using several approaches. First we try the
simplest, taking advantage of the @OverloadedStrings@ extension to write

> let metadata = [("samp.name", "dummy"),
>                 ("samp.description.text", "Test Application"),
>                 ("dummy.version", "0.1-3")]

Whilst the above is simple, it will result in run-time errors if the input
text is invalid (i.e. includes invalid characters). To explicitly handle conversion
errors we could say

> let mdkeys = [("samp.name", "dummy"),
>               ("samp.description.text", "Test Application"),
>               ("dummy.version", "0.1-3")]
> case handleError Left (mapM (uncurry stringToKeyValE) mdkeys) of
>     Left emsg -> hPutStrLn stderr emsg >> exitFailure
>     Right metadata -> ...

-}

{- $basic 

From the SAMP documentation:

> # Read information from lockfile to locate and register with hub.
> string hub-url = readFromLockfile("samp.hub.xmlprc.url");
> string samp-secret = readFromLockfile("samp.secret");
>
> # Establish XML-RPC connection with hub
> # (uses some generic XML-RPC library)
> xmlrpcServer hub = xmlrpcConnect(hub-url);
>
> # Register with hub.
> map reg-info = hub.xmlrpcCall("samp.hub.register", samp-secret);
> string private-key = reg-info.getValue("samp.private-key");
>
> # Store metadata in hub for use by other applications.
> map metadata = ("samp.name" -> "dummy",
>                 "samp.description.text" -> "Test Application",
>                 "dummy.version" -> "0.1-3");
> hub.xmlrpcCall("samp.hub.declareMetadata", private-key, metadata);
>
> # Send a message requesting file load to all other 
> # registered clients, not wanting any response.
> map loadParams = ("filename" -> "/tmp/foo.bar");
> map loadMsg = ("samp.mtype" -> "file.load",
>                "samp.params" -> loadParams);
> hub.xmlrpcCall("samp.hub.notifyAll", private-key, loadMsg);
>
> # Unregister
> hub.xmlrpcCall("samp.hub.unregister", private-key);

which could be written as

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

-}

{- $errors

In this example we add a simple handler that will ensure the client is
unregistered from the hub even if the
@'Control.Exception.UserInterrupt'@ exception is thrown.

The program accepts a list of mtypes on the command line and lists all
the clients that are subscribed to each mtype.

> module Main where
>
> import System.Environment (getArgs, getProgName)
> import System.Exit (exitFailure)
> import System.IO
>
> import qualified Control.Exception as CE
> import Control.Monad (forM_)
> import Control.Monad.Trans (liftIO)
>
> import Network.SAMP.Standard 
>
> usage :: IO ()
> usage = getProgName >>= \n -> hPutStrLn stderr ("Usage: " ++ n ++ " mtype1 .. mtypeN")

> main :: IO ()
> main = do
>     args <- getArgs
>     if null args 
>       then usage >> exitFailure
>       else do
>         -- the conversion routines like @toMTypeE@ are safe to run
>         -- outside of @withSAMP@.
>         mtypes <- runE (mapM toMTypeE args)
>         withSAMP $ doSAMP mtypes

> doSAMP :: [MType] -> IO ()
> doSAMP mtypes = do
>     -- register the client
>     conn <- runE $ getHubInfoE >>= registerClientE
>     putStrLn "Registered with the SAMP hub."
>
>     -- catch a user interrupt so that we can unregister the client
>     let hdlr :: CE.AsyncException -> IO a
>         hdlr CE.UserInterrupt = runE (unregisterE conn) >> CE.throwIO CE.UserInterrupt
>         hdlr e = CE.throwIO e
>
>     -- Awkward error handling to make sure we unregister on an error. It is
>     -- not quite correct.
>     handleError (\m -> runE (unregisterE conn) >> fail m)
>                 (act conn mtypes) `CE.catch` hdlr
>     runE $ unregisterE conn
>     putStrLn "Unregistered client."
>
> -- Report on clients that are subscribed to the given message.
>
> report :: SAMPConnection -> MType -> Err IO ()
> report conn msg = do
>     liftIO $ putStrLn $ "Clients subscribed to " ++ show msg
>     clients <- getSubscribedClientsE conn msg
>     forM_ clients $ \(cl,_) -> do
>         liftIO $ putStr $ "  " ++ fromRString cl
>         name <- getClientNameE conn cl
>         case name of
>             Just n -> liftIO $ putStr $ " -> " ++ fromRString n
>             _ -> return ()
>         liftIO $ putStr "\n"
>
> act :: SAMPConnection -> [MType] -> Err IO ()
> act conn = mapM_ (report conn)

Here is an example of a successful run:

> unix% ./hsamp-errorclient table.load.votable samp.app.ping
> Registered with the SAMP hub
> What clients are subscribed to: table.load.votable
>   c9 -> topcat
>   c12 -> SAOImage DS9
> What clients are subscribed to: samp.app.ping
>   c9 -> topcat
>   c12 -> SAOImage DS9
>   hub -> Hub
> Unregistered client

and several unsuccesful ones:

  1) An invalid MType (only lower-case characters are allowed).

> unix% ./hsamp-errorclient SAMP.app.ping
> hsamp-errorclient: user error (Invalid MType 'SAMP.app.ping' (invalid character 'S'/0x53))

  2) Using a wild-card in an MType is not supported by the
     @getSubscribedClients@ method.

> unix% ./hsamp-errorclient samp.\*
> Registered with the SAMP hub
> What clients are subscribed to: samp.*
> hsamp-errorclient: user error (MType can not contain a wild card when calling getSubscribedClients)

  3) No SAMP Hub is running.

> unix% ./hsamp-errorclient samp.app.ping
> hsamp-errorclient: user error (The SAMP Hub lock file does not exist: /Users/dburke/.samp)

-}
