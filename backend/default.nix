{ pkgs, _expose ? false }:

let
  project = pkgs.haskell-nix.stackProject {
    src = with pkgs.haskell-nix.haskellLib; cleanSourceWith {
      name = "edna";
      src = cleanGit {
        src = ../.;
        subDir = "backend";
      };
    };
    modules = [
      ({ pkgs, ... }: {
        packages = {
          edna = {
            components.tests.edna-test = {
              # These are runtime deps, but there is nowhere else to put them
              build-tools = with pkgs; [
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

          # FIXME: Probably fails only on older snapshots
          base-noprelude.components.library.doHaddock = false;
          bytestring-builder.components.library.doHaddock = false;
          co-log-sys.components.library.doHaddock = false;

          # FIXME haddock fails on loot-log, remove the following line when fixed
          loot-log.components.library.doHaddock = false;
        };
      })
    ];
    # Something weird for older snapshots. Copied from haskell.nix examples.
    pkg-def-extras = [
      (hackage: {
        packages = {
          "hsc2hs" = (((hackage.hsc2hs)."0.68.4").revisions).default;
        };
      })
    ];
  };
in

if _expose
then project
else pkgs.haskell-nix.haskellLib.selectLocalPackages project
