
help:
	@echo "Targets are:"
	@echo "    testclient"

testclient:	.dummy
	@ghc -Wall --make -o testclient -threaded -hide-package monads-fd TestClient.hs
	@echo "Created: testclient"

.dummy:

