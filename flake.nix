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

    nix-npm-buildpackage = {
      url = "github:serokell/nix-npm-buildpackage";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, haskell-nix, hackage, stackage, nix-npm-buildpackage, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        inherit (nixpkgs.lib) foldl' composeExtensions;

        pkgs = nixpkgs.legacyPackages.${system}.extend
          (foldl' composeExtensions (_: _: { }) [
            nix-npm-buildpackage.overlay
            haskell-nix.overlay
          ]);

        backend = pkgs.callPackage ./backend { };
        frontend = pkgs.callPackage ./frontend { };
        frontendCheck = checkPhase:
          frontend.package.overrideAttrs (_: {
            doBuild = false;
            doCheck = true;
            inherit checkPhase;
          });
      in {
        defaultPackage = self.packages.${system}.backend-server;
        packages = {
          backend-lib = backend.library;
          backend-server = backend.server;
          frontend = frontend.package;
        };

        checks = {
          backend-test = backend.test;
          frontend-tscompile = frontendCheck "yarn run tscompile";
          frontend-tslint = frontendCheck "yarn run tslint";
          frontend-stylelint = frontendCheck "yarn run stylelint";
        };

        devShell = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.backend-lib ];
          buildInputs = with pkgs.haskellPackages; [ cabal-install hpack hlint ];
        };
      });
}
