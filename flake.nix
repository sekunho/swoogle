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

        # Loaded automatically into shell by `direnv` + `nix-direnv`. You could
        # also use `nix develop` if you want.
        #
        # `nix-direnv`: https://github.com/nix-community/nix-direnv
        devShell = pkgs.mkShell rec {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          buildInputs = [
            # pkgs.musl
            # unstable.ghc
            unstable.ghcid
            unstable.haskell.compiler.integer-simple.ghc8107

            pkgs.cabal-install # Build tool
            unstable.haskell-language-server # Haskell LSP
            pkgs.hlint # Linter
            pkgs.haskellPackages.implicit-hie # To deal with HLS + cabal oddities
            pkgs.haskellPackages.stylish-haskell # Code formatter
            pkgs.haskellPackages.stan # Idk how to use this yet
            pkgs.haskell-ci # Github Actions generator

            # Front-end
            unstable.nodePackages.tailwindcss # Styling with utility classes
            unstable.esbuild # Node.JS? What's that?

            # Deploy
            pkgs.flyctl # Fly's CLI for deploy

            # Misc.
            pkgs.watchexec # Watch changes and execute something

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
