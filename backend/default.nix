{ haskell-nix }:
let
  project = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit {
      src = haskell-nix.cleanSourceHaskell { src = ./.; };
    };
  };

  library = project.edna.components.library;
  server = project.edna.components.exes.edna-server;
  test = project.edna.checks.edna-test;

in {
  inherit server library test;
}
