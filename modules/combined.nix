{ config, pkgs, lib, ... }:
let
  cfg = config.services.bridge;
in {
  imports = [ ./frontend.nix ./backend.nix ];
  options.services.bridge = {
    enable = lib.mkEnableOption "Stakerdao bridge frontend and backend combined with some sane defaults";
  };
  config = lib.mkIf cfg.enable {
    services.bridge.frontend = {
      enable = true;
      api_addr = "http://${config.services.bridge.backend.config.api.listen_addr}";
    };
    services.bridge.backend = {
      enable = true;
      configurePostgres = true;
    };
  };
}
