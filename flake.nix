{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      eachSystem = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          packages = [
            pkgs.cabal-install
            pkgs.fzf
            pkgs.gh
            pkgs.ghc
            pkgs.haskell-language-server
            pkgs.haskellPackages.cabal-gild
            pkgs.hlint
            pkgs.jq
            pkgs.nixfmt
            pkgs.ormolu
            pkgs.pkg-config
            pkgs.ripgrep
          ];
          # zlib is a build input (not just a tool) so that pkg-config can
          # discover it via PKG_CONFIG_PATH; the zlib Haskell package, a
          # transitive dependency of cabal-install, needs it to configure.
          buildInputs = [
            pkgs.zlib
          ];
        };
      });
    };
}
