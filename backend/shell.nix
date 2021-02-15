let
  pkgs = import ../nix {};
  project = import ./. { inherit pkgs; _expose = true; };
in

project.shellFor {}
