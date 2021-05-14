<!--
   - SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
   -
   - SPDX-License-Identifier: AGPL-3.0-or-later
   -->

# Edna Web App

[![License: AGPL-3.0-or-later](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

## Build

If you are building the project for the first time, you need to run `yarn install`.
Also, you need to run `yarn install` if a new dependency is added to `package.json`.

`yarn build` builds bundles in production mode.

## Run

`yarn serve` runs the dev server.
You need to launch backend on `9000` port for the server to operate correctly as specified in [webpack.dev.js](./webpack.dev.js).
If you make any changes, `yarn serve` will automatically trigger recompilation.

## Lint

`yarn tslint` runs linter.
