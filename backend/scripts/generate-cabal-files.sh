#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2020 Tocqueville Group
#
# SPDX-License-Identifier: MIT

# This script generates cabal.project, cabal.project.freeze files.

set -e

[ -f cabal.project ] && rm cabal.project
[ -f cabal.project.freeze ] && rm cabal.project.freeze

mkdir -p stack2cabal
stack install stack2cabal --resolver snapshot-stack2cabal.yaml --local-bin-path stack2cabal
./stack2cabal/stack2cabal
rm -rf stack2cabal
echo "cabal.project and cabal.project.freeze files are succesfully generated"
