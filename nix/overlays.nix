{ sources ? import ./sources.nix }:

[(self: super: {
  inherit (self.callPackage sources.nix-npm-buildpackage {}) buildNpmPackage;
  inherit (self.callPackage sources.gitignore {}) gitignoreSource gitignoreFilter;

  /*
  * Run a series of commands only for their exit status, producing an empty
  * closure.
  */
  runCheck = script: src:  self.runCommand "check" {} ''
    src="${src}"
    ${script}
    touch $out
  '';
})] ++ (import sources.haskell-nix).overlays
