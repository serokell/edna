# Edna server

Web server (backend) for Edna.

## Build instructions

You can use [Stack](http://haskellstack.org/) or [Cabal](https://www.haskell.org/cabal/) to build Edna server.
Run `stack build` or `cabal build`.

We also provide a [Makefile](./Makefile) to facilitate building and testing during development.

## Tests instructions

To run tests you need to have [`pg_tmp`](http://eradman.com/ephemeralpg/) to run DB for tests.
Then run `make test` to execute tests.

## Usage instructions

Depending on the tool you used to build `edna-server` you need to run of these commands:
* `stack exec -- edna-server -c dev-config.yaml`
* `cabal run -- edna-server -c dev-config.yaml`

You can update `dev-config.yaml` as you wish before running.
If you use the config from this repository:
* The server endpoints will be available at `http://localhost:9000/api/*`.
* Swagger docs will be available at `http://localhost:9000/docs/`.

Note that you need to also run PostgreSQL server to make backend work.
One way to run is to use `docker-compose.yml` provided in the [deployment](../deployment) folder.

Set `EDNA_DEBUG_DB=1` to enable logging of all DB actions made.
It works in tests as well.
