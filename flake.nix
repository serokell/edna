# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

{
  inputs = {
    nixpkgs.url = "github:serokell/nixpkgs";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    serokell-nix.url = "github:serokell/serokell.nix";

    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";

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

      # nixpkgs has an older version of stack2cabal which doesn't build
      # with new libraries, use a newer version
      packages.stack2cabal = (pkgs.haskellPackages.callHackageDirect {
        pkg = "stack2cabal";
        ver = "1.0.11";
        sha256 = "00vn1sjrsgagqhdzswh9jg0cgzdgwadnh02i2fcif9kr5h0khfw9";
      } { }).overrideAttrs (o: {
        src = pkgs.fetchFromGitHub {
          owner = "hasufell";
          repo = "stack2cabal";
          rev = "afa113beb77569ff21f03fade6ce39edc109598d";
          sha256 = "1zwg1xkqxn5b9mmqafg87rmgln47zsmpgdkly165xdzg38smhmng";
        };
        version = "1.0.12";
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
