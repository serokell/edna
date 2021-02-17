{ config, pkgs, lib, ... }:
let
  cfg = config.services.bridge.frontend;
in {
  options.services.bridge.frontend = {
    enable = lib.mkEnableOption "blend frontend serving via nginx";
    package = lib.mkOption {
      description = "Path to the frontend package";
      type = lib.types.path;
      default = pkgs.bridge-web.frontend;
    };
    api_addr = lib.mkOption {
      description = "Address of API";
      type = lib.types.str;
    };
    fqdn = lib.mkOption {
      description = "FQDN for the app to use";
      type = lib.types.str;
      default = "${config.networking.hostName}.${config.networking.domain}";
    };
  };
  config = lib.mkIf cfg.enable {
    services.nginx = {
      enable = true;
      virtualHosts.bridge = {
        default = true;
        serverName = cfg.fqdn;
        forceSSL = true;
        enableACME = true;
        locations = {
          "/api/".proxyPass = "${cfg.api_addr}/";
          "/static/".alias = "${cfg.package}/";
          "/" = {
            root = cfg.package;
            tryFiles = "/index.html =404";
            extraConfig = "add_header Cache-Control no-cache;";
          };
        };
      };
    };
  };
}
