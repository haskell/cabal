{
  description = "A devshell that enables nix users to just run `cabal build cabal` and start contributing to Cabal";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        packages.default = pkgs.lib.trace "This flake currently only contains a devshell for use with nix develop";
        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.ghc pkgs.cabal-install pkgs.zlib ];
        };
      };
      flake = {};
    };
}
