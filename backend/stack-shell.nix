let
  pkgs = import ../nix {};
  project = import ./. { inherit pkgs; _expose = true; };
in

# stack needs only the compiler, it will manage the dependencies itself.
project.shellFor {
  packages = ps: [ ];
  withHoogle = false;

  # FIXME: there is some issue with stm in haskell.nix, which result it not
  # being included in stack's resolver configuration. So we add it manually.
  # It is a bootstrap library anyway.
  additional = ps: [ ps.stm ];

  # FIXME: Could be added automatically by haskell.nix.
  buildInputs = with pkgs; [
    cacert
    git
    pkgconfig

    lzma
    postgresql
    zlib
  ];

  # Workaround for https://gitlab.haskell.org/ghc/ghc/issues/11042
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lzma.out}/lib:${pkgs.zlib.out}/lib:$LD_LIBRARY_PATH
  '';
}
