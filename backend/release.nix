{ pkgs ? import ./../nix {} }: with pkgs;

let
  project = import ./. { inherit pkgs; };
in
{
  edna-backend = symlinkJoin {
    name = "edna-backend";
    paths = lib.attrValues project.edna.components.exes;
  };
}
