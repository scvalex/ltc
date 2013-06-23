LTc
===

> A replicated data store for high-latency disconnected environments

Build
=====

LTc requires at least `ghc- 7.6.1` and `cabal-install` to start
building and, downloads the rest of the dependencies as it needs them.

The simplest way to build LTc is through the `Makefile`:

    % make

Alternatively, you can explicitly call `cabal-install`:

    % cabal install --enable-tests

A few tests are always run after every build by the `Makefile`.  In
order to run the full test suite, use one of the following commands:

    % make test
    % cabal test

Run
===

The simplest way to run LTc is through the debugging program `ltc`.
For instance, to start a node with the Redis adapter enabled, run:

    % ./ltc redis

You can now connect to the node with any compatible Redis client.

Alternatively, you can start a test node with the web interface
enabled by running:

    % ./ltc node

You can now see the web interface at
[http://localhost:5000](http://localhost:5000).

See the help options for the `ltc` program for details.

Embed
=====

LTc is normally meant to be used as an embedded data store, but an
example use is beyond the scope of this readme.  See `ltc-tool.hs` and
the programs in `examples/` for code samples.
