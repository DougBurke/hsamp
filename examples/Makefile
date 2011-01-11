
GHC = ghc -Wall -threaded -hide-package monads-fd -i..

help:
	@echo "Targets are:"
	@echo "    all"
	@echo ""
	@echo "    sender"
	@echo "    snooper"
	@echo "    errorclient"
	@echo "    basicclient"

all:	basicclient errorclient snooper sender 

sender:	.dummy
	@$(GHC) --make -o sender Sender.lhs
	@echo "Created: sender"

snooper:	.dummy
	@$(GHC) --make -o snooper Snooper.lhs
	@echo "Created: snooper"

basicclient:	.dummy
	@$(GHC) --make -o basicclient BasicClient.lhs
	@echo "Created: basicclient"

errorclient:	.dummy
	@$(GHC) --make -o errorclient ErrorClient.lhs
	@echo "Created: errorclient"

.dummy:

