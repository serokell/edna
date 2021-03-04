{ buildYarnPackage, fetchurl, python2 }:

buildYarnPackage {
  SASS_BINARY_PATH = fetchurl {
    url = "https://github.com/sass/node-sass/releases/download/v4.14.1/linux-x64-64_binding.node";
    sha256 = "0g0dbccs1wdib1nm7cg6x3iqhxfnrk69cnlsqff2d52gcwymmacj";
  };

  src = ./.;
  buildInputs = [
    python2
  ];

  yarnBuildMore = ''
    yarn build
  '';

  installPhase = ''
    mv dist $out
  '';
}
