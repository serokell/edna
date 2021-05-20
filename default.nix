# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: CC0-1.0

let
  flake = (import (fetchTarball
    "https://github.com/edolstra/flake-compat/archive/master.tar.gz") {
      src = builtins.fetchGit ./.;
    }).defaultNix;
in flake // flake.packages.${builtins.currentSystem}
