# Edna server

Web server for Edna.

## Build instructions

You can use [Stack](http://haskellstack.org/) or [Cabal](https://www.haskell.org/cabal/) to build Edna server.
Run `stack build` or `cabal build`.

We also provide a [Makefile](./Makefile) to facilitate building and testing during development.

## Tests instructions

To run tests you need to have [`pg_tmp`](http://eradman.com/ephemeralpg/) to run DB for tests.
Then run `make test` to execute tests.

## Usage instructions

Run `stack exec -- edna-server -c dev-config.yaml` or `cabal run -- edna-server -c dev-config.yaml` depending on the build tool you used.
* The server endpoints will be available at `http://localhost:9000/api/*`.
* Swagger docs will be available at `http://localhost:9000/docs/`.

Set `EDNA_DEBUG_DB=1` to enable logging of all DB actions made.
It works in tests as well.
