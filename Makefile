
help:
	@echo "Targets are:"
	@echo "    testclient"

testclient:	TestClient.hs SAMP/Types.hs SAMP/Client.hs
	@ghc -Wall --make -o testclient -threaded -hide-package monads-fd TestClient.hs
	@echo "Created: testclient"

