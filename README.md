<!--
   - SPDX-FileCopyrightText: 2019-2021 Serokell <https://serokell.io>
   -
   - SPDX-License-Identifier: LicenseRef-ReplaceMe
   -->

# Edna

<!--
TODO: CI and license badges!

[![License: MPL 2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://opensource.org/licenses/MPL-2.0)
   -->

Edna is an open-source tool for data analysis aimed to help researchers with their experiments.

## Build Instructions

There is nothing to build yet.

## Usage

To run a postgres container run `docker-compose up postgres` in `deployment` directory,
then you can connect to it via `psql` or using any convenient database viewer.

To run a server run `stack exec -- edna-server -c config.yaml`:
* The server endpoints available at `localhost:9000/api/*`.
* Swagger docs are available at `http://localhost:9000/docs/`.

## Issue Tracker

We use our own issue tracker.
If you want to open a new issue, please do so on [GitHub](https://github.com/serokell/edna/issues/new/choose).

## For Contributors

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## About Serokell

Edna is maintained and funded with ❤️ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
