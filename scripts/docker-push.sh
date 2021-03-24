#!/usr/bin/env bash

skopeo copy --dest-creds=serokell-bot:"$GITHUB_TOKEN" "$@"
