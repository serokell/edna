{ dockerTools
, backend
, frontend
, locale
, glibcLocales
, linkFarm
, runCommand
, writeTextDir }:

let
  inherit (dockerTools) buildLayeredImage buildImage pullImage;
in

{
  backend-image = let
    # linkFarm seems to put a dangling symlink in the image when used here, so
    # we use runCommand and copy the file instead.
    init-sql = runCommand "edna-init-sql" {} ''
      mkdir -p $out
      cp ${./backend/sql/init.sql} $out/init.sql
    '';
  in buildLayeredImage {
    name = "ghcr.io/serokell/edna-backend";
    tag = "latest";

    contents = [
      backend
      init-sql

      # Unicode support
      locale
      glibcLocales
    ];

    config = {
      Entrypoint = "/bin/edna-server";
      Env = [
        "LANG=en_US.UTF-8"
        "LC_ALL=en_US.UTF-8"
        "LOCALE_ARCHIVE=/lib/locale/locale-archive"
      ];
    };
  };

  frontend-image = let
    nginx = pullImage {
      imageName = "nginx";
      imageDigest = "sha256:e20c21e530f914fb6a95a755924b1cbf71f039372e94ac5ddcf8c3b386a44615";
      sha256 = "1xfl1yx5mklxzcbywmfr8y48kkkp7if364r24ghjz33jkb3wm8vy";
      finalImageName = "nginx";
      finalImageTag = "alpine";
    };
    html = linkFarm "edna-frontend-bundle" [{
      name = "usr/share/nginx/html";
      path = "${frontend}";
    }];
    nginx-conf-template = writeTextDir "etc/nginx/templates/default.conf.template" ''
      server {
        listen 80 default_server;
        root /usr/share/nginx/html;
        location / {
          try_files $uri /index.html =404;
        }

        location ''${API_PATH} {
          proxy_pass http://''${API_HOST}:''${API_PORT}/;
        }

        location /health/ {
          return 204;
        }
      }
    '';
  in buildImage {
    name = "ghcr.io/serokell/edna-frontend";
    tag = "latest";
    fromImage = nginx;
    contents = [
      html
      nginx-conf-template
    ];

    config = {
      # Somehow, all this gets lost when inheriting from the parent image.
      Entrypoint = [ "/docker-entrypoint.sh" ];
      Cmd = [ "nginx" "-g" "daemon off;" ];
      ExposedPorts = {
        "80/tcp" = {};
      };

      # Used in the conf template above
      Env = [
        "API_PATH=/api/"
        "API_HOST=backend"
        "API_PORT=9000"
      ];
    };
  };
}
