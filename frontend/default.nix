{ buildYarnPackage, fetchurl, python2 }:
let
  package = buildYarnPackage {
    SASS_BINARY_PATH = fetchurl {
      url = "https://github.com/sass/node-sass/releases/download/v4.12.0/linux-x64-64_binding.node";
      sha256 = "0dl91l414na44h090cgghd06q0j2whlj9h98im2qb9823glq7xff";
    };
    buildInputs = [ python2 ];

    src = ./.;
    yarnBuild = "yarn";
  };
in {
  inherit package;
}
