{ haskell-nix, ephemeralpg }:
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
          components.tests.edna-test = {
            ghcOptions = ["-Werror"];
            # These are runtime deps, but there is nowhere else to put them
            build-tools = [
              ephemeralpg
            ];
            preCheck = ''
              export TEST_PG_CONN_STRING=$(pg_tmp -w 600)
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
