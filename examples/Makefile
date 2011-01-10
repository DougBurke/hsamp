
GHC = ghc -Wall -threaded -hide-package monads-fd -i..

help:
	@echo "Targets are:"
	@echo "    snooper"
	@echo "    sender"
	@echo "    basicclient"
	@echo "    errorclient"

snooper:	.dummy
	@$(GHC) --make -o snooper Snooper.lhs
	@echo "Created: snooper"

sender:	.dummy
	@$(GHC) --make -o sender Sender.lhs
	@echo "Created: sender"

basicclient:	.dummy
	@$(GHC) --make -o basicclient BasicClient.lhs
	@echo "Created: basicclient"

errorclient:	.dummy
	@$(GHC) --make -o errorclient ErrorClient.lhs
	@echo "Created: errorclient"

.dummy:

