# Edna server

HTTP API backend service for Edna.

## Build instructions

You can use [Stack](http://haskellstack.org/) or [Cabal](https://www.haskell.org/cabal/) to build Edna server.
Run `stack build` or `cabal build`.

We also provide a [Makefile](./Makefile) to facilitate building and testing during development.
By default it uses `stack`, but you can set `EDNA_USE_CABAL=1` to use `cabal`.

## Usage instructions

`edna-server` calls Python code under the hood which has some dependencies.
So first of all you need to enter [`poetry`](https://python-poetry.org/) shell with all dependencies available.
`poetry` commands should run from the folder with Python code, so start with [`cd ../analysis`](../analysis).
There, do `poetry install` to install dependencies and then `poetry shell` to make them available.

Depending on the tool you used to build `edna-server` you need to run of these commands:
* `stack exec -- edna-server -c dev-config.yaml`
* `cabal run -- edna-server -c dev-config.yaml`

Alternatively, you can use `make run` command which does what's written above.
Set `RUN_ARGUMENTS` to command line options you want to pass.
Example: `RUN_ARGUMENTS='-c dev-config.yaml' make run`.

You can update `dev-config.yaml` as you wish before running.
If you use the config from this repository:
* The server endpoints will be available at `http://localhost:9000/api/*`.
* Swagger docs will be available at `http://localhost:9000/docs/`.
* Data for prometheus will be available at `http://localhost:9000/metrics/`.

Note that you need to also run PostgreSQL server to make backend work.
One way to run it is to use `docker-compose.yml` provided in the [deployment](../deployment) folder.

Set `EDNA_DB_DEBUG=1` to enable logging of all DB actions made.
It works in tests as well.

You can pass other command line arguments, to see full list run `stack exec -- edna-server -h` or `cabal run -- edna-server -h`:

You can use environment variables to configure your server (available variables listed in [`.env`](../deployment/.env)).

NOTE: priority of configurations is following: command line arguments, environment variables, config file.

## Tests instructions

To run tests you need to have [`pg_tmp`](http://eradman.com/ephemeralpg/) to run DB for tests.
Then run `make test` to execute tests.
This command automatically executes [`poetry`](https://python-poetry.org/) to obtain Python dependencies.

## Generate sample data

Apart from `edna-server` executable we provide `edna-generator`.
This tool can generate sample data for Edna and put it into DB.
You can build and run it the same way as `edna-server` (via `stack` or `cabal`).
You can also run it using [`Makefile`](./Makefile).
It has the same options as `edna-server` plus some additional options that specify how much data to generate.
Pass `--help` to get the full list of available options.

You are adviced to set logging to `prod` or `nothing` (otherwise there will be too much output in logs)
Also maybe you also want to run it on empty DB.
Example with `prod` logging and empty DB: `RUN_ARGUMENTS='-c dev-config.yaml -l prod --init-mode enable-with-drop' make run-generator`.
If DB is not empty and there are conflicts (such as already existing project name), by default they will be skipped.

Note that `edna-generator` is an experimental tool for developers and may be unstable/unreliable sometimes.
