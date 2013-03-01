CABAL := $(shell cabal-dev --version > /dev/null && echo cabal-dev || echo cabal)

all: build test

.PHONY: all build dist install clean doc site p ghci stores

build: dist/setup-config
	rm -rf _site _cache
	$(CABAL) build

dist:
	$(CABAL) sdist

install: build
	cabal install

clean:
	$(CABAL) clean
	rm -rf cabal-dev/ test-store/

dist/setup-config: ltc.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	$(CABAL) configure --enable-tests || $(CABAL) install --enable-tests

doc: build
	$(CABAL) haddock

test: build
	$(CABAL) test

p:
	permamake.sh $(shell find src/ -name '*.hs') \
	             $(shell find test/ -name '*.hs') \
	             ltc-tool.hs \
	             *.cabal \
	             Makefile

ghci: build
	cabal-dev ghci

stores: build
	rm -rf *-store
	./ltc populate -c 10 some-store
	./ltc info some-store
