{ sources ? import ./sources.nix }:
let
  overlays = import ./overlays.nix { inherit sources; };

in

import sources.nixpkgs { inherit overlays; config = {}; }
