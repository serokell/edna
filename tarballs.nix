{ dockerTools, backend, frontend, runCommand, bash, cacert, iana_etc, coreutils }: {
  backend = let
    db = ./backend/db;
  in
    dockerTools.buildLayeredImage {
    name = "bridge-web-backend";
    tag = "latest";
    contents = [ backend bash cacert iana_etc db coreutils ];
    config.Entrypoint = "edna-server";
  };
  frontend = runCommand "frontend.tar.gz" { } ''
    tar -chva -f $out -C ${frontend} .
  '';
}
