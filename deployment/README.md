<!--
SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>

SPDX-License-Identifier: AGPL-3.0-or-later
-->

# Edna deployment

The [docker-compose configuration file](./docker-compose.yml) declares how to deploy Edna using images from GitHub Container Registry:
* [backend](https://github.com/orgs/serokell/packages/container/package/edna-backend)
* [frontend](https://github.com/orgs/serokell/packages/container/package/edna-frontend)

You can use `docker-compose` to run all 3 images (`postgres`, `backend` and `frontend`) or any individual image.
For example:
* `docker-compose up postgres` to run `postgres` in the foreground.
* `docker-compose up --no-start` to create all services without running them.
* `docker-compose start` to run all services in the background.

If you run all 3 images (by not passing any image or passing `frontend`), you can open `127.0.0.1:8080` in your browser and use Edna.

By default `docker-compose` will pull Edna images from GitHub Container Registry.
However, if you import an image manually, it will be used instead.
You can use `docker-compose pull` to update all images.

Note: we use `.env` with default options, if you want to change it - create a new file (e. g. `.env.local`) and run `docker-compose --env-file=.env.local start`.

## Configuration

`edna-server` supports 3 ways of configuration: YAML configuration file, env variables and command line options.
Here we use the first two:
* [configuration file](./config.yaml) is mounted to `/config.yaml` and read by `edna-server` inside the image.
* [.env](./.env) file defines a number of env variables propagated to the backend and other images:
  + `POSTGRES_*` options affect both `postgres` and `backend` containers and should be self-descripting.
  `backend` uses them to construct a connection string to connect to the DB.
  + `EDNA_*` options match their counterparts from the configuration file.

These are the most significant configuration options you may want to change:
* `EDNA_API_LISTEN_ADDR` is the network address where the backend server will be bound.
* `EDNA_API_SERVE_DOCS` specifies whether Swagger UI will be accessible at `/docs`.
* `EDNA_DB_INITIALISATION_MODE` can be set to `enable` (to run the initialisation script), `enable-with-drop` (to drop the whole DB and run the initialisation script) or omitted.
* `EDNA_LOGGING` can be set to `dev` for the most verbose logging, `prod` for non-verbose loggig and `nothing` to disable logging.
* `EDNA_MDE_HOST` specifies the prefix used to construct MDe links for compounds (compound number is appended to the value of this option after `/`).
