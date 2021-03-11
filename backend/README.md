# Edna server

Web server for Edna.

## Build instructions

You can use [Stack](http://haskellstack.org/) or [Cabal](https://www.haskell.org/cabal/) to build Edna server.
Run `stack build` or `cabal build`.

## Usage instructions

Run `stack exec -- edna-server -c config.yaml` or `cabal run -- edna-server -c config.yaml` depending on the build tool you used.
* The server endpoints will be available at `http://localhost:9000/api/*`.
* Swagger docs will be available at `http://localhost:9000/docs/`.
