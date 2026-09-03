all : build-using-stack

build-using-stack:
	stack build --flag bk:debug

build-using-cabal:
	cabal build -f "debug"

prod-build-using-stack:
	stack build

prod-build-using-cabal:
	cabal build