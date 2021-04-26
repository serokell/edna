<!--
   - SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
   -
   - SPDX-License-Identifier: AGPL-3.0-or-later
   -->

# Edna Web App

[![License: AGPL-3.0-or-later](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

## Build

If you are building project at the first time, you need to run `yarn install`.

Also, you need to run `yarn install` if a new dependency is added to `package.json`.

`yarn build` builds bundles.

## Run

`yarn serve` runs the dev server.
To operate correctly, it's needed to have backend launched on `8080` port.

## Lint

`yarn tslint` runs linter.
