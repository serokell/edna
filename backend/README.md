# Edna server

HTTP API backend service for Edna.

## Build instructions

You can use [Stack](http://haskellstack.org/) or [Cabal](https://www.haskell.org/cabal/) to build Edna server.
Run `stack build` or `cabal build`.

We also provide a [Makefile](./Makefile) to facilitate building and testing during development.

## Usage instructions

`edna-server` calls Python code under the hood which has some dependencies.
So first of all you need to enter [`poetry`](https://python-poetry.org/) shell with all dependencies available.
`poetry` commands should run from the folder with Python code, so start with [`cd ../analysis`](../analysis).
There, do `poetry install` to install dependencies and then `poetry shell` to make them available.

Depending on the tool you used to build `edna-server` you need to run of these commands:
* `stack exec -- edna-server -c dev-config.yaml`
* `cabal run -- edna-server -c dev-config.yaml`

You can update `dev-config.yaml` as you wish before running.
If you use the config from this repository:
* The server endpoints will be available at `http://localhost:9000/api/*`.
* Swagger docs will be available at `http://localhost:9000/docs/`.

Note that you need to also run PostgreSQL server to make backend work.
One way to run it is to use `docker-compose.yml` provided in the [deployment](../deployment) folder.

Set `EDNA_DEBUG_DB=1` to enable logging of all DB actions made.
It works in tests as well.

## Tests instructions

To run tests you need to have [`pg_tmp`](http://eradman.com/ephemeralpg/) to run DB for tests.
Then run `make test` to execute tests.
This command automatically executes [`poetry`](https://python-poetry.org/) to obtain Python dependencies.
