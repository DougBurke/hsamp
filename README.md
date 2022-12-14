
This is currently a vanity project, but I'd like to turn it into
something useful, if I had time ...

# Can we access SAMP via Haskell?

http://www.ivoa.net/Documents/REC/App/SAMP-20090421.html

http://software.astrogrid.org/doc/p/jsamp/1.0/

http://hackage.haskell.org/packages/archive/haxr/3000.2.1/doc/html/Network-XmlRpc-Client.html

Need to update to the latest SAMP standard!

# Building

It can be built with `cabal` or `stack`. I currently don't guarantee that
the "old" stack versions will work; they are kept around in case they are
useful - e.g.

```
% STACK_YAML=stack-ghc-8.0.yaml stack build
```

# A quick run through

```
% cabal new-repl
In order, the following will be built (use -v for more details):
hsamp-0.3
Preprocessing library hsamp-0.3...
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
[1 of 6] Compiling Network.SAMP.Standard.Types ( src/Network/SAMP/Standard/Types.hs, interpreted )
[2 of 6] Compiling Network.SAMP.Standard.Setup ( src/Network/SAMP/Standard/Setup.hs, interpreted )
[3 of 6] Compiling Network.SAMP.Standard.Server.Scotty ( src/Network/SAMP/Standard/Server/Scotty.hs, interpreted )
[4 of 6] Compiling Network.SAMP.Standard.Client ( src/Network/SAMP/Standard/Client.hs, interpreted )
[5 of 6] Compiling Network.SAMP.Standard.Server ( src/Network/SAMP/Standard/Server.hs, interpreted )
[6 of 6] Compiling Network.SAMP.Standard ( src/Network/SAMP/Standard.hs, interpreted )
Ok, modules loaded: Network.SAMP.Standard, Network.SAMP.Standard.Client, Network.SAMP.Standard.Server, Network.SAMP.Standard.Server.Scotty, Network.SAMP.Standard.Setup, Network.SAMP.Standard.Types.
*Network.SAMP.Standard> hi <- runE (sampLockFileE >>= getHubInfoE)
*Network.SAMP.Standard> :t hi
hi :: SAMPInfo

*Network.SAMP.Standard> getSAMPInfoHubURL hi
"http://127.0.0.1:44945/xmlrpc/"
*Network.SAMP.Standard> getSAMPInfoHubSecret hi
"oPvFqEHTC03gplua"
*Network.SAMP.Standard> getSAMPInfoHubMetadata hi
fromList [("samp.profile.version","1.2")]

*Network.SAMP.Standard> :set prompt "hsamp> "
hsamp> conn <- runE (registerClientE hi)
hsamp> :t conn
conn :: SAMPConnection

hsamp> ns <- runE (getRegisteredClientsE conn)
hsamp> :t ns
ns :: [ClientName]
hsamp> ns
["hub","cl1"]
hsamp> scId conn
"cl2"

hsamp> :set -XOverloadedStrings
hsamp> mdhub <- runE (getMetadataE conn "hub")
hsamp> :t mdhub
mdhub :: SAMPMapValue
hsamp> mdhub
fromList [("author.affiliation",SAMPString "Chandra X-ray Center, Smithsonian Astrophysical Observatory"),("author.mail",SAMPString "dburke@cfa.harvard.edu"),("author.name",SAMPString "Douglas Burke"),("samp.description.text",SAMPString "An example SAMP hub, written in Haskell."),("samp.icon.url",SAMPString "http://127.0.0.1:44945/logo.png"),("samp.name",SAMPString "hsamp-hub")]
hsamp> Data.Map.lookup "samp.name" mdhub
Just (SAMPString "hsamp-hub")

hsamp> md1 <- runE (getMetadataE conn "cl1")
hsamp> md1
fromList [("author.affiliation",SAMPString "Smithsonian Astrophysical Observatory"),("author.email",SAMPString "dburke.gw@gmail.com"),("author.name",SAMPString "Doug Burke"),("samp.description.text",SAMPString "Report on messages sent by the hub."),("samp.name",SAMPString "hsamp-snooper")]

hsamp> runE (getMetadataE conn "clx")
*** Exception: user error (user error (Error 1: Invalid client id: "clx"))

hsamp> subs <- runE (getSubscriptionsE conn "cl1")
hsamp> :t subs
subs :: [(MType, SAMPValue)]
hsamp> subs
[(*,SAMPMap (fromList []))]

hsamp> rsp <- runE (getSubscribedClientsE conn "samp.app.ping")
hsamp> :t rsp
rsp :: [(ClientName, SAMPValue)]
hsamp> rsp
[("cl1",SAMPMap (fromList [])),("hub",SAMPMap (fromList []))]
```

# Doing it old school

```
*Network.XmlRpc.Client> let url = "http://127.0.0.1:55071/xmlrpc"
*Network.XmlRpc.Client> let secret = "8fe977378fa002e6"
*Network.XmlRpc.Client> let regClient :: String -> IO [(String, String)] ; regClient = remote url "samp.hub.register"
*Network.XmlRpc.Client> :t regClient
regClient :: String -> IO [(String, String)]
*Network.XmlRpc.Client> ans <- regClient secret
*Network.XmlRpc.Client> ans
[("samp.hub-id","hub"),("samp.self-id","c1"),("samp.private-key","k:2_pplstclcoslgqbiu")]
```

so is the work we do in Network.SAMP.Standard.Client really necessary?
Well, yes, since all it really does is wrap this up.

# TODO/THINK ABOUT

  - Where is the slow down coming from in the test code; it involves the
    serialization of the message to or from XML (I think) but where
    exactly and whose "fault" is it (mine, some library code, ...)?
    
  - move server code from examples (snooper/sender) to Network.SAMP.Standard.Server.Snap
    and add in *.HappStack/*.WAI versions. Hmm, maybe not WAI since the interface
    can abstract around the HTTP server, so perhaps *.WARP.

  - is it worth making a monad based on StateT to carry around connection info; this
    can then periodically ping the hub/check for the hub disappearing/re-appearing
    to allow it to handle hub replacements?

  - is it worth keeping the *E versions, or just work in the IO monad?

  - can we encode a type to ensure that the Server routines for a client (ASync
    Call) are only used when a server is actually set up? I don't think this is
    actually useful, just conceptually interesting for me

# Copyright

SPDX short identifier: BSD-3-Clause

This software is released under the 3-clause BSD License, which can be
found in the LICENSE file. See also
https://opensource.org/licenses/BSD-3-Clause
