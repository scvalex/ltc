CABAL := $(shell cabal-dev --version > /dev/null && echo cabal-dev || echo cabal)
RESOURCES := www/r/rickshaw.min.css www/r/rickshaw.min.js www/r/d3.v3.min.js www/r/mootools-yui-compressed.js www/r/knockout-2.2.1.js

all: build resources fasttest

.PHONY: all build dist install clean doc site p ghci stores

build: dist/setup-config
	rm -rf _site _cache
	$(CABAL) build

dist: build test
	$(CABAL) sdist

install: build
	cabal install

clean:
	$(CABAL) clean
	rm -rf cabal-dev/ *store*/ $(RESOURCES)

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

fasttest: build
	$(cabal) test diff

resources: $(RESOURCES)

www/r/rickshaw.min.css:
	wget -O $@ https://raw.github.com/shutterstock/rickshaw/master/rickshaw.min.css

www/r/rickshaw.min.js:
	wget -O $@ https://raw.github.com/shutterstock/rickshaw/master/rickshaw.min.js

www/r/d3.v3.min.js:
	wget -O $@ http://d3js.org/d3.v3.min.js

www/r/mootools-yui-compressed.js:
	wget -O $@ http://ajax.googleapis.com/ajax/libs/mootools/1.4.5/mootools-yui-compressed.js

www/r/knockout-2.2.1.js:
	wget -O $@ http://ajax.aspnetcdn.com/ajax/knockout/knockout-2.2.1.js
