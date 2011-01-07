
help:
	@echo "Targets are:"
	@echo "    testclient"
	@echo "    docs"

testclient:	.dummy
	@ghc -Wall --make -o testclient -threaded -hide-package monads-fd TestClient.hs
	@echo "Created: testclient"

docs:	.dummy
	@rm -rf html
	@haddock -h -o html --optghc="-hide-package monads-fd" Network/SAMP/*hs Network/SAMP/Standard/*hs

.dummy:

