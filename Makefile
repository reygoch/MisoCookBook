.PHONY: BuildAll Settings AppServer DataLayer RunDevServer

BuildAll:
	cabal new-build all

Settings:
	ghcid -c "cabal new-repl Settings"  \
  --restart=.\\cabal.project           \
  --restart=.\\Settings\\Settings.cabal \

AppServer:
	ghcid -c "cabal new-repl AppServer"   \
  --restart=.\\cabal.project             \
  --restart=.\\AppServer\\AppServer.cabal \

DataLayer:
	ghcid -c "cabal new-repl DataLayer"   \
  --restart=.\\cabal.project             \
  --restart=.\\DataLayer\\DataLayer.cabal \

DevServer:
	ghcid -c "cabal new-repl AppServer"   \
  --restart=.\\cabal.project             \
  --restart=.\\AppServer\\AppServer.cabal \
  -T Cookster.DevServer.start              \
