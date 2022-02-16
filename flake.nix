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
            pkgs.ghc
            # pkgs.haskell.compiler.integer-simple.ghc8107

            # I had to wrestle with `cabal`'s resolver and so I decided to switch.
            # I will look into using `haskell.nix`, but only when I get my fiber
            # internet back.
            pkgs.stack                           # Build tool
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

            # Set `~/.stack/config.yaml` with this:
            # ```yaml
            #   nix:
            #     enable: true
            #     packages: [zlib.dev, zlib.out]
            # ```
            #
            # Otherwise it'll complain about `zlib` while building.
            # https://github.com/commercialhaskell/stack/issues/2975
            pkgs.zlib
            ];

          LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
        };
      });
}
