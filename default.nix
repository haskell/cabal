with (import <nixpkgs> {});
let
  inherit (haskell) lib;
  filterSource = drv:  # only copy required source files to build directory
    let
      omitDirs = [ ".cabal-sandbox" ".git" "dist" ];
      omitExts = [ ".o" ".hi" ];
      hasExt = path: ext: stdenv.lib.hasSuffix ext path;
      predicate = path: type:
        if type == "directory"
          then !(stdenv.lib.elem (baseNameOf path) omitDirs)
          else !(stdenv.lib.any (hasExt path) omitExts);
    in
      lib.overrideCabal drv
      (args: args // { src = builtins.filterSource predicate args.src; });
in
haskellPackages.override {
  overrides = self: super: {
    Cabal = filterSource (self.callPackage ./Cabal.nix {});
    cabal-install = filterSource (lib.dontCheck (self.callPackage ./cabal-install.nix {}));
  };
}
