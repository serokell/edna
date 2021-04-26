#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

skopeo copy --dest-creds=serokell-bot:"$GITHUB_TOKEN" "$@"
