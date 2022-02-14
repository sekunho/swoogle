{
  description = "SWAPI";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    unstablepkgs.url    = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, unstablepkgs }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = nixpkgs.lib;
        unstable = unstablepkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell rec {
          buildInputs = [
            pkgs.ghc                             # Haskell compiler
            pkgs.cabal-install                   # Build tool
            pkgs.haskell-language-server         # Haskell LSP
            pkgs.hlint                           # Linter
            pkgs.haskellPackages.implicit-hie    # To deal with HLS + cabal oddities
            pkgs.haskellPackages.stylish-haskell # Code formatter
            pkgs.haskell-ci                      # Github Actions generator
            pkgs.zlib                            # I forgot why this was here lmao

            # Front-end
            unstable.nodePackages.tailwindcss

            # Misc.
            pkgs.watchexec
          ];

          LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
        };
      });
}
