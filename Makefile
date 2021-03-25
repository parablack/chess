all: *.hs
	ghc -outputdir /tmp/ -dynamic -O2 main.hs

test: *.hs
	ghc -outputdir /tmp/ -dynamic -O2 tests.hs
	./tests

watch:
	fd -e hs | entr make