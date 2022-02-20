{
  description = ''
    Clients & client libraries for the Star Wars API.
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    unstablepkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, flake-utils, unstablepkgs, pre-commit-hooks }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = nixpkgs.lib;
        unstable = unstablepkgs.legacyPackages.${system};
      in
      {
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              cabal-fmt.enable = true;
              # hlint.enable = true;

              my-stylish-haskell = {
                enable = true;
                entry = "${pkgs.haskellPackages.stylish-haskell}/bin/stylish-haskell --config .stylish-haskell.yml --inplace";
                files = "\\.l?hs$";
              };
            };
          };
        };

        devShells.test = pkgs.mkShell rec {
          buildInputs = [
            unstable.haskell.compiler.integer-simple.ghc8107
            unstable.cabal-install
            pkgs.zlib
          ];

          shellHook = "cat motd/test.txt";
          LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
        };

        # Loaded automatically into shell by `direnv` + `nix-direnv`. You could
        # also use `nix develop` if you want.
        #
        # `nix-direnv`: https://github.com/nix-community/nix-direnv
        devShell = pkgs.mkShell rec {
          shellHook = ''
            ${self.checks.${system}.pre-commit-check.shellHook}
            cat motd/dev.txt
          '';

          buildInputs = [
            ## HASKELL TOOLS

            # I use this to auto-reload swoogle-server
            unstable.ghcid

            # Glorious Glasgow Haskell Compiler, without integer-gmp
            unstable.haskell.compiler.integer-simple.ghc8107

            # Haskell build tool
            pkgs.cabal-install

            # Haskell LSP
            unstable.haskell-language-server

            # Haskell Linter
            pkgs.hlint

            # To appease HLS
            pkgs.haskellPackages.implicit-hie

            # Code formatter so I can take care of a lot of mental overhead
            pkgs.haskellPackages.stylish-haskell

            # Static code analyzer
            pkgs.haskellPackages.stan

            # Github Actions generator; I'm not really using this these days.
            pkgs.haskell-ci

            ## FRONT-END TOOLS

            # Styling with CSS utility classes
            unstable.nodePackages.tailwindcss

            # Because I'm sick and tired of Node.JS and its nth "fundamentally
            # different" bundler/framework when they're all slow, and annoying.
            # Why bother roping in an entire JS runtime just to bundle assets?
            unstable.esbuild

            ## MISC.

            # For deploying applications
            pkgs.flyctl

            # I can't remember the exact flags to use so I just stuffed 'em in a
            # Makefile
            pkgs.gnumake

            # A shared library that GHC requires
            pkgs.zlib
          ];

          LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
        };
      });
}
