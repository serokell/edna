{ haskell-nix, runCommand, lib }:
let
  project = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit {
      src = haskell-nix.cleanSourceHaskell { src = ./.; };
    };
    modules = [
      {
        doHaddock = false;
        packages.edna = {
          doHaddock = true;
          package.ghcOptions = "-Werror";
        };
      }
      {
        packages = builtins.listToAttrs (map (name: {
          inherit name;
          value.postUnpack = "cp -Lr --remove-destination ${./hpack.yaml} */hpack.yaml";
        }) [
          "staker-bridge-core"
          "staker-block-sync"
          "staker-bridge-app"
          "staker-bridge-db"
          "staker-bridge-tez"
          "edna"
        ]);
      }
      {
        packages.staker-bridge-eth = {
          postUnpack = "
            cp -Lr --remove-destination ${./hpack.yaml} */hpack.yaml;
            rm -f */resources/abi;
            cp -Lr --remove-destination ${../resources}/abi */resources/;
            ";
        };
      }
    ];
  };
  library = project.edna.components.library;
  exes = project.edna.components.exes;
  server = exes.edna-server;
  swagger-gen = exes.swagger-gen;
  eth-approval = exes.eth-approval;
  swagger-file = runCommand "swagger.yaml" {
    LANG = "C.UTF-8";
    buildInputs = [ swagger-gen ];
  } "mkdir -p $out; swagger-gen > $out/swagger.yaml";
  tez-test = project.staker-bridge-tez.checks.test;
in { inherit server swagger-file tez-test library eth-approval; }
