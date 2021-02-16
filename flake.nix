{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:serokell/nixpkgs";
    haskell-nix = {
      url =
        "github:input-output-hk/haskell.nix/bd45da822d2dccdbb3f65d0b52dd2a91fd65ca4e";
    };
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
    deploy-rs.url = "github:serokell/deploy-rs";
    common-infra.url = "github:serokell/common-infra";
  };

  outputs = { self, nixpkgs, haskell-nix, hackage, stackage
    , nix-npm-buildpackage, deploy-rs, common-infra }:
    let
      packagesFor = system: rec {
        pkgs = nixpkgs.legacyPackages.${system}.extend
          (nixpkgs.lib.foldl nixpkgs.lib.composeExtensions (_: _: { }) [
            nix-npm-buildpackage.overlay
            (haskell-nix.overlays {
              sourcesOverride = haskell-nix.sources // {
                inherit hackage stackage;
              };
            }).combined-eval-on-build
          ]);
        backend = pkgs.callPackage ./backend { };
        frontend =
          pkgs.callPackage ./frontend { swagger-file = backend.swagger-file; };

        tarballs = pkgs.callPackage ./tarballs.nix {
          backend = backend.server;
          frontend = frontend.package;
        };
        backend-image = tarballs.backend // { meta.artifacts = [ "" ]; };
        frontend-tarball = tarballs.frontend // { meta.artifacts = [ "" ]; };
        frontendCheck = checkPhase:
          frontend.package.overrideAttrs (_: {
            doBuild = false;
            doCheck = true;
            inherit checkPhase;
          });
      };
    in {
      packages = builtins.mapAttrs (system: _:
        with packagesFor system; {
          frontend = frontend.package;
          openapi-client = frontend.openapi-client;
          openapi-client-archive = frontend.openapi-client-archive // {
            meta.artifacts = [ "/openapi-client-archive.zip" ];
          };
          backend-lib = backend.library;
          backend = backend.server // {
            meta.modulePath = [ "services" "bridge" "backend" ];
          };
          swagger-file = backend.swagger-file // {
            meta.artifacts = [ "/swagger.yaml" ];
          };
          eth-approval = backend.eth-approval;
          inherit backend-image frontend-tarball;
        }) nixpkgs.legacyPackages;

      ciSystems = [ "x86_64-linux" ];

      checks = builtins.mapAttrs (system: _:
        with packagesFor system; {
          tez-test = backend.tez-test;
          frontend-tscompile = frontendCheck "npm run tscompile";
          frontend-tslint = frontendCheck "npm run tslint";
          frontend-stylelint = frontendCheck "npm run stylelint";
          frontend-test = frontendCheck "npm run test";
        }) nixpkgs.legacyPackages;

      nixosModules = {
        backend = import ./modules/backend.nix;
        frontend = import ./modules/frontend.nix;
        combined = import ./modules/combined.nix;
      };

      pipelineFile = common-infra.mkPipelineFile self;

      deployFromPipeline = [
        {
          branch = "testing";
          profile = "backend";
        }
        {
          branch = "testing";
          profile = "frontend";
        }
      ];

      deploy = let
        mkNode = hostname: {
          inherit hostname;
          user = "deploy";
          profiles = {
            backend.path = deploy-rs.lib.x86_64-linux.activate.custom
              self.packages.x86_64-linux.backend
              "sudo /run/current-system/sw/bin/systemctl restart bridge";
            frontend.path = deploy-rs.lib.x86_64-linux.activate.noop
              self.packages.x86_64-linux.frontend;
          };
        };
      in {
        sshOpts = [ "-p 17788" ];
        nodes = {
          testing = mkNode "bridge.stakerdao.serokell.team";
        };
      };

      apps.x86_64-linux.deploy = deploy-rs.defaultApp.x86_64-linux;

      devShell = builtins.mapAttrs (system: packages:
        let pkgs = nixpkgs.legacyPackages.${system};
        in pkgs.mkShell {
          inputsFrom = [ packages.backend-lib packages.frontend ];
          buildInputs = with pkgs.haskellPackages; [ cabal-install hpack ];
        }) self.packages;
    };
}
