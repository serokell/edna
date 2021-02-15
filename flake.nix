{
  inputs = {
    nixpkgs.url = "github:serokell/nixpkgs";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    stackage = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, hackage, stackage, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend haskell-nix.overlay;
        backend = pkgs.callPackage ./backend { };
      in {
        defaultPackage = self.packages.${system}.backend-server;
        packages = {
          backend-lib = backend.library;
          backend-server = backend.server;
        };

        checks.backend-test = backend.test;

        devShell = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.backend-lib ];
          buildInputs = with pkgs.haskellPackages; [ cabal-install hpack hlint ];
        };
      });
}
