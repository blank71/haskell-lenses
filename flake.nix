# https://docs.haskellstack.org/en/v3.9.3/topics/nix_integration/#supporting-both-nix-and-non-nix-developers
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nixpkgs-20-09.url = "github:NixOS/nixpkgs/release-20.09";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-20-09,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgs-20-09 = nixpkgs-20-09.legacyPackages.${system};
        inherit (pkgs-20-09) haskell;

        # Use the default GHC from nixpkgs to avoid removed compiler sets.
        # If you need a specific GHC, switch to pkgs.haskell.packages.ghcXYZ.
        # hPkgs = pkgs.haskellPackages"; # no specific GHC version, will use the default one from nixpkgs
        hPkgs = haskell.packages.ghc865;

        myDevTools = [
          # For install ghc-8.6.5 through stack
          pkgs-20-09.gmp
          pkgs-20-09.libffi
          pkgs-20-09.ncurses5

          # external deps for haskell-lenses
          ## postgresql-simple
          # pkgs.postgresql
          # pkgs.postgresql.pg_config

          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          # hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.ormolu # Haskell formatter
          pkgs.haskellPackages.cabal-fmt # Haskell formatter for cabal files
          # hPkgs.hlint # Haskell codestyle checker
          # hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          # hPkgs.retrie # Haskell refactoring tool
          # hPkgs.cabal-install
          stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages

          pkgs.rust-analyzer
        ];

        # Wrap Stack to work with our Nix integration. We do not want to modify
        # stack.yaml so non-Nix users do not notice anything.
        # - no-nix: We do not want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Do not try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      }
    );
}
