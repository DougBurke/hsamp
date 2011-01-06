
help:
	@echo "Targets are:"
	@echo "    testclient"

testclient:	TestClient.hs SAMP/Standard/Types.hs SAMP/Standard/Client.hs SAMP/Standard/Server.hs
	@ghc -Wall --make -o testclient -threaded -hide-package monads-fd TestClient.hs
	@echo "Created: testclient"

