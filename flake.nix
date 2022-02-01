{
  description = "SWAPI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = nixpkgs.lib;
      in {
        devShell = pkgs.mkShell rec {
          buildInputs = with pkgs; [
            ghc                             # Haskell compiler
            cabal-install                   # Build tool
            haskell-language-server         # Haskell LSP
            hlint                           # Linter
            haskellPackages.implicit-hie    # To deal with HLS + cabal oddities
            haskellPackages.stylish-haskell # Code formatter
            haskell-ci                      # Github Actions generator
            zlib                            # I forgot why this was here lmao
          ];

          LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
        };
      });
}
