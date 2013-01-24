all: build test

.PHONY: all build dist install clean doc site p

build: dist/setup-config
	rm -rf _site _cache
	cabal-dev build

dist:
	cabal-dev sdist

install: build
	cabal install

clean:
	cabal-dev clean
	rm -rf cabal-dev/ test-store/

dist/setup-config: ltc.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	cabal-dev configure --enable-tests || cabal-dev install --enable-tests

doc: build
	cabal-dev haddock

test: build
	cabal-dev test

p:
	permamake.sh $(shell find Ltc/ -name '*.hs') $(shell find Test/ -name '*.hs') *.cabal Makefile
