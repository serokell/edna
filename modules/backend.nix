{ config, pkgs, lib, ... }:
let
  cfg = config.services.bridge.backend;
  fromYAML = yaml:
    builtins.fromJSON (builtins.readFile (pkgs.runCommand "from-yaml.json" { }
      "cat ${yaml} | ${pkgs.yaml2json}/bin/yaml2json > $out"));

  # Throw if a config value is unset
  # We can't just do `unsetValue = path: throw ...` because recursiveUpdate
  # will lazily evaluate it and that results in a throw. If we add a proxy
  # attrset, lazily evaluating it doesn't throw, but deeply evaluating it
  # (which toJSON does) throws.
  unsetValue = path: {
    fakeValue = throw "You haven't set services.bridge.backend.config.${
        builtins.concatStringsSep "." path
      }";
  };

  # Throw if a config value is null
  throwForNulls = lib.mapAttrsRecursive
    (path: value: if isNull value then unsetValue path else value);

  defaultConfig = fromYAML ../backend/server/config.yaml;
in {
  options.services.bridge.backend = {
    enable = lib.mkEnableOption "stakerdao bridge backend";
    user = lib.mkOption {
      description = "User with which bridge will be ran";
      type = lib.types.str;
      default = "blend";
    };
    package = lib.mkOption {
      description = "Path to the backend package";
      type = lib.types.path;
      default = pkgs.bridge-web.backend;
    };
    serviceName = lib.mkOption {
      description = "Name of systemd service";
      default = "bridge-web";
    };

    configurePostgres = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description =
        "Whether to configure a postgresql database for stakerdao bridge";
    };

    secretFile = lib.mkOption {
      type = lib.types.path;
      description = ''
        A file containing secrets:
        - BRIDGE_SECRET_SEED
        - BRIDGE_SECRET_ETH
        - BRIDGE_SECRET_TEZ
      '';
    };

    config = lib.mkOption rec {
      description = "Configuration for bridge backend";
      type = lib.types.attrs;
      apply = lib.recursiveUpdate (throwForNulls default);
      default = lib.recursiveUpdate defaultConfig {
        api = {
          listen_addr = "127.0.0.1:8190";
          serve_docs = true;
        };
        chain.ethereum.https_provider = "https://goerli.infura.io/v3/4960731a79c3498cb1813aa1a47e9d12";
        db = {
          conn_string = null;
          migration.script_dir = ../backend/server/db/migration;
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = { isSystemUser = true; };
    systemd.services.${cfg.serviceName} = rec {
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = cfg.user;
        ExecStart = "${cfg.package}/bin/edna-server -c ${
            builtins.toFile "config.yml" (builtins.toJSON cfg.config)
          }";
        Restart = "always";
        StateDirectory = "bridge-web";
        EnvironmentFile = cfg.secretFile;
      };
    };

    services.bridge.backend.config.db.conn_string =
      if cfg.configurePostgres then
        "host=/run/postgresql dbname=bridge"
      else
        null;

    services.postgresql = lib.mkIf cfg.configurePostgres {
      enable = true;
      ensureUsers = [{
        name = cfg.user;
        ensurePermissions = { "DATABASE bridge" = "ALL PRIVILEGES"; };
      }];
      ensureDatabases = [ "bridge" ];
    };
  };
}
