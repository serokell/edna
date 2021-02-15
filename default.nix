{ pkgs ? import ./nix {} }: with pkgs;
let
  # Helpers to build system derivations
  shim = {
    boot.loader.systemd-boot.enable = true;

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/00000000-0000-0000-0000-000000000000";
      fsType = "btrfs";
    };
  };

  buildSystem = config: (import "${pkgs.path}/nixos" {
    configuration = { imports = [ config shim ]; };
  }).system;

  # Import project derivations
  backend  = import ./backend/release.nix { inherit pkgs; };
in

{
  inherit (backend)
    edna-backend;
}
