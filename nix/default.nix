{ sources ? import ./sources.nix }:
import sources.nixpkgs { inherit sources; config = {}; }
