<!--
   - SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
   -
   - SPDX-License-Identifier: AGPL-3.0-or-later
   -->

# Edna


[![License: AGPL-3.0-or-later](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

Edna is an open-source web application for data analysis aimed to help researchers with their experiments.
It consists of 3 parts:
* [backend](/backend/) is a web server implemented in Haskell.
* [frontend](/frontend/) is a web client application (UI) implemented in TypeScript.
* [analysis](/analysis/) is a part of the backend implemented in Python to analyse data submitted to Edna.

## Usage

The recommended way to deploy Edna is to use Docker images from GitHub Container Registry:
* [backend](https://github.com/orgs/serokell/packages/container/package/edna-backend)
* [frontend](https://github.com/orgs/serokell/packages/container/package/edna-frontend)

Note that some additional configuration is needed: you need to launch PostgreSQL DB server, forward ports and provide appropriate configuration (via a config file or environment variables or command line arguments).
The [`deployment`](deployment/) folder contains an example [`docker-compose`](deployment/docker-compose.yml) file that automates the whole deployment.
Please read its [README](/deployment/README.md) for more details about using the images.

## Build Instructions

If the above way is not suitable for you, you can build Edna from sources and run it.
You need to build and run both [backend](./backend) and [frontend](./frontend).
Please refer to the respective directories for more details.

## Issue Tracker

We use our own issue tracker.
If you want to open a new issue, please do so on [GitHub](https://github.com/serokell/edna/issues/new/choose).

## For Contributors

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## About Serokell

Edna is maintained and funded with ❤️ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
