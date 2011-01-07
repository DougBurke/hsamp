{-|

Support for the SAMP Standard Profile.

From the SAMP documentation:

>   # Read information from lockfile to locate and register with hub.
>   string hub-url = readFromLockfile("samp.hub.xmlprc.url");
>   string samp-secret = readFromLockfile("samp.secret");
>
>   # Establish XML-RPC connection with hub
>   # (uses some generic XML-RPC library)
>   xmlrpcServer hub = xmlrpcConnect(hub-url);
>
>   # Register with hub.
>   map reg-info = hub.xmlrpcCall("samp.hub.register", samp-secret);
>   string private-key = reg-info.getValue("samp.private-key");
>
>   # Store metadata in hub for use by other applications.
>   map metadata = ("samp.name" -> "dummy",
>                   "samp.description.text" -> "Test Application",
>                   "dummy.version" -> "0.1-3");
>   hub.xmlrpcCall("samp.hub.declareMetadata", private-key, metadata);
>
>   # Send a message requesting file load to all other 
>   # registered clients, not wanting any response.
>   map loadParams = ("filename" -> "/tmp/foo.bar");
>   map loadMsg = ("samp.mtype" -> "file.load",
>                  "samp.params" -> loadParams);
>   hub.xmlrpcCall("samp.hub.notifyAll", private-key, loadMsg);
>
>   # Unregister
>   hub.xmlrpcCall("samp.hub.unregister", private-key);

which could be written as

-}

module Network.SAMP.Standard (
       module Network.SAMP.Standard.Types,
       module Network.SAMP.Standard.Client,
       module Network.SAMP.Standard.Server
       ) where
       
import Network.SAMP.Standard.Types
import Network.SAMP.Standard.Client
import Network.SAMP.Standard.Server
