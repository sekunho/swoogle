{
  description = ''
  Clients & client libraries for the Star Wars API.
  '';

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs";
    flake-utils.url  = "github:numtide/flake-utils";
    unstablepkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, unstablepkgs }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = nixpkgs.lib;
        unstable = unstablepkgs.legacyPackages.${system};
      in {
        # Loaded automatically into shell by `direnv` + `nix-direnv`. You could
        # also use `nix develop` if you want.
        #
        # `nix-direnv`: https://github.com/nix-community/nix-direnv
        devShell = pkgs.mkShell rec {
          buildInputs = [
            pkgs.ghc                             # Glorious Glasgow Haskell compiler

            # Tooling
            pkgs.cabal-install                   # Build tool
            pkgs.haskell-language-server         # Haskell LSP
            pkgs.hlint                           # Linter
            pkgs.haskellPackages.implicit-hie    # To deal with HLS + cabal oddities
            pkgs.haskellPackages.stylish-haskell # Code formatter
            pkgs.haskell-ci                      # Github Actions generator

            # Front-end
            unstable.nodePackages.tailwindcss    # Styling with utility classes

            # Deploy
            pkgs.flyctl                          # Fly's CLI for deploy

            # Misc.
            pkgs.watchexec                       # Watch changes and execute something
            pkgs.zlib
            ];

          LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
        };
      });
}
