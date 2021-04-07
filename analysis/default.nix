{ poetry2nix }:
poetry2nix.mkPoetryEnv {
  projectDir = ./.;
  overrides = [
    poetry2nix.defaultPoetryOverrides
    (self: super: {
      matplotlib = super.matplotlib.overrideAttrs (oa:
      let
        qhull = builtins.fetchTarball {
          url = "http://www.qhull.org/download/qhull-2020-src-8.0.2.tgz";
          sha256 = "sha256:1ak865nfgj84j1cq84lxa9v8qalprffmlkaw09krfpbm5dzqymby";
        };
      in
      {
        propagatedBuildInputs = oa.propagatedBuildInputs ++ [ self.certifi ];

        preBuild = ''
          mkdir -p /build/matplotlib-3.4.1/build
          cp -Lr ${qhull} /build/matplotlib-3.4.1/build/qhull-2020.2
        '';
      });
    })
  ];
}
