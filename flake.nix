# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

{
  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    haskell-nix = {
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
    };
    hackage.flake = false;
    stackage.flake = false;
  };

  outputs = { self, nixpkgs, haskell-nix, serokell-nix, deploy-rs, hackage, stackage, nix-npm-buildpackage, flake-utils }:
  nixpkgs.lib.recursiveUpdate
  (let
    system = "x86_64-linux";
    inherit (nixpkgs.lib) mapAttrs;
    inherit (nixpkgs.legacyPackages.${system}) linkFarm;
    mkEdnaNode = hostName:
      let
        profile = with self.packages.${system};
          linkFarm "edna-deploy-profile" [
            { name = "backend.tar.gz";
            path = docker-backend { }; }
            { name = "frontend.tar.gz";
            path = docker-frontend { }; }
          ];
      in {
        hostname = "${hostName}.edna.serokell.team";
        sshOpts = [ "-p" "17788" ];
        profiles.edna-docker = {
          sshUser = "deploy";
          path = deploy-rs.lib.${system}.activate.custom profile
            ''
              sudo systemctl restart docker-backend
              sudo systemctl restart docker-frontend
            '';
        };
      };
  in {
    # Do not roll back profile closure deployment
    deploy.magicRollback = false;

    deploy.nodes.staging = mkEdnaNode "staging";
    deploy.nodes.demo = mkEdnaNode "demo";

    checks = mapAttrs (_: lib: lib.deployChecks self.deploy) deploy-rs.lib;
  })

  (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      inherit (serokell-nix.lib) pkgsWith;

      pkgs = pkgsWith nixpkgs.legacyPackages.${system} [
        nix-npm-buildpackage.overlay
        haskell-nix.overlay
        serokell-nix.overlay
      ];

      analysis-env = pkgs.callPackage ./analysis { };
      backend = pkgs.callPackage ./backend {
        inherit analysis-env;
        EDNA_ANALYSIS_DIR = ./analysis;
      };
      docker = creationDate: pkgs.callPackage ./docker.nix {
        backend = backend.server;
        inherit frontend;
        creationDate = creationDate;
      };

      frontend = pkgs.callPackage ./frontend { };
      frontendCheck = checkPhase:
        frontend.overrideAttrs (_: {
          doBuild = false;
          doCheck = true;
          inherit checkPhase;
        });

    in {
      defaultPackage = self.packages.${system}.backend-server;
      packages = {
        analysis-env = analysis-env;
        backend-lib = backend.library;
        backend-server = backend.server;
        docker-backend = { creationDate ? "1970-01-01T00:00:01Z" }: (docker creationDate).backend-image;
        docker-frontend = { creationDate ? "1970-01-01T00:00:01Z" }: (docker creationDate).frontend-image;
        frontend = frontend;
      };

      checks = {
        backend-test = backend.test;
        frontend-stylelint = frontendCheck "yarn run stylelint";
        frontend-tscompile = frontendCheck "yarn run tscompile";
        frontend-tslint = frontendCheck "yarn run tslint";
      };

      # stack2cabal is broken because of strict constraints, set 'jailbreak' to ignore them
      packages.stack2cabal = pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.stack2cabal (drv: {
        jailbreak = true;
        broken = false;
      });

      devShell = pkgs.mkShell {
        inputsFrom = [ self.packages.${system}.backend-lib ];
        buildInputs = with pkgs.haskellPackages; [
          cabal-install hpack hlint self.packages.${system}.stack2cabal
          deploy-rs.defaultPackage.${system}
          pkgs.skopeo
          pkgs.reuse
          pkgs.bats
          backend.server
          self.packages.${system}.analysis-env
        ];
      };
    }));
}
