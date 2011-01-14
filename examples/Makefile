
GHC = ghc -Wall -threaded -hide-package monads-fd -hide-package monads-tf -i..

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
	@$(GHC) --make -o hsamp-sender Sender.lhs
	@echo "Created: hsamp-sender"

snooper:	.dummy
	@$(GHC) --make -o hsamp-snooper Snooper.lhs
	@echo "Created: hsamp-snooper"

basicclient:	.dummy
	@$(GHC) --make -o hsamp-basicclient BasicClient.lhs
	@echo "Created: hsamp-basicclient"

errorclient:	.dummy
	@$(GHC) --make -o hsamp-errorclient ErrorClient.lhs
	@echo "Created: hsamp-errorclient"

.dummy:

