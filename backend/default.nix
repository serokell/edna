# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

{ haskell-nix, ephemeralpg, lib, makeWrapper, analysis-env, EDNA_ANALYSIS_DIR }:
let

  project = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit {
      src = haskell-nix.cleanSourceHaskell { src = ./.; };
    };
    modules = [{
      doHaddock = true;
      packages = {
        edna = {
          doHaddock = true;
          ghcOptions = ["-Werror"];

          components.exes.edna-server = {
            # Do not depend on GHC
            dontStrip = false;
            build-tools = [ makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/edna-server \
                --prefix PATH : '${lib.makeBinPath [ analysis-env ]}' \
                --set EDNA_ANALYSIS_DIR '${EDNA_ANALYSIS_DIR}'
            '';
          };

          components.tests.edna-test = {
            ghcOptions = ["-Werror"];
            # These are runtime deps, but there is nowhere else to put them
            build-tools = [
              ephemeralpg
              analysis-env
            ];
            preCheck = ''
              export TEST_PG_CONN_STRING=$(pg_tmp -w 600)
              export EDNA_ANALYSIS_DIR='${EDNA_ANALYSIS_DIR}'
            '';
            # we need the temporary directory from pg_tmp
            # so extract it out of $TEST_PG_CONN_STRING
            postCheck = ''
              pg_tmp stop -d $(echo ''${TEST_PG_CONN_STRING#*=} | sed 's:%2F:/:g') || :
            '';
          };
        };
      };
    }];
  };

  library = project.edna.components.library;
  server = project.edna.components.exes.edna-server;
  test = project.edna.checks.edna-test;

in {
  inherit server library test;
}
