# https://docs.haskellstack.org/en/v3.9.3/topics/nix_integration/#supporting-both-nix-and-non-nix-developers
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nixpkgs-release.url = "github:NixOS/nixpkgs/release-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      nixpkgs,
      nixpkgs-release,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgs-release = nixpkgs-release.legacyPackages.${system};
        inherit (pkgs-release) haskell;

        # Use the default GHC from nixpkgs to avoid removed compiler sets.
        # If you need a specific GHC, switch to pkgs.haskell.packages.ghcXYZ.
        # hPkgs = pkgs.haskellPackages"; # no specific GHC version, will use the default one from nixpkgs
        hPkgs = haskell.packages.ghc8107;

        myDevTools = [
          # For install ghc-8.6.5 through stack
          pkgs-release.gmp
          pkgs-release.libffi
          pkgs-release.ncurses5

          # external deps for postgresql-simple
          pkgs-release.postgresql
          # pkgs.postgresql.pg_config # if use nixos-unstable, pg_config is not available in pkgs.postgresql

          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          # hPkgs.ghcid # Continuous terminal Haskell compile checker
          # hPkgs.ormolu # Haskell formatter
          pkgs.haskellPackages.cabal-fmt # Haskell formatter for cabal files
          # hPkgs.hlint # Haskell codestyle checker
          # hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          # hPkgs.retrie # Haskell refactoring tool
          # hPkgs.cabal-install
          stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages

          ### for local postgres server
          db-init
          db-start
          db-stop
          db-chech-env
          psql-wrapped
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

        ### custom script to run a local postgres server
        ### https://github.com/hermann-p/nix-postgres-dev-db/tree/3d81eebf90584c1d9cc659b3c8c0f67bf2832335
        root-env-var = "$PG_ROOT";
        db-path = "${root-env-var}/.db";
        db-name = "links";
        db-user = "links";
        db-passwd = "links";
        db-port = "5432";
        db-script-name = "haskell-lenses-db";

        db-chech-env = pkgs.writeShellScriptBin "${db-script-name}-check" ''
          if [[ -z "${root-env-var}" ]]; then
            echo '${root-env-var}' is not set, can not init database;
            exit 1;
          fi
        '';

        db-init = pkgs.writeShellScriptBin "${db-script-name}-init" ''
          set -e
          ${db-chech-env}/bin/${db-script-name}-check
          db_pid_dir="/run/postgresql"
          current_user=$(id -u -n)

          initdb -D "${db-path}"
          pg_ctl -D ${db-path} -o "-k ${db-path}" -o "-p ${db-port}" -l ${db-path}/database.log start
          for DBNAME in {${db-name},$current_user}; do
            createdb -h ${db-path} -p ${db-port} $DBNAME
          done
          psql --host ${db-path} --port ${db-port} \
            -tc "CREATE USER ${db-user} WITH SUPERUSER"
        '';

        db-start = pkgs.writeShellScriptBin "${db-script-name}-start" ''
          set -e
          ${db-chech-env}/bin/${db-script-name}-check
          if [[ ! -d "${db-path}" ]]; then
            ${db-init}/bin/${db-script-name}-init
          elif [[ ! -f "${db-path}/.s.PGSQL.${db-port}.lock" ]]; then
              pg_ctl -D ${db-path} -o "-k ${db-path}" -o "-p ${db-port}" -l ${db-path}/database.log start
          else
              echo Postgres is already running for this project
          fi
        '';

        db-stop = pkgs.writeShellScriptBin "${db-script-name}-stop" ''
          set -e
          ${db-chech-env}/bin/${db-script-name}-check
          pg_ctl -D ${db-path} stop || rm "${db-path}/.s.PGSQL.${db-port}.lock"
        '';

        psql-wrapped = pkgs.writeShellScriptBin "psql-wrapped" ''
          set -e
          ${db-chech-env}/bin/${db-script-name}-check
          PGHOST="${db-path}" PGPORT="${db-port}" PGUSER="${db-user}" PGPASSWORD="${db-passwd}" PGDATABASE="${db-name}" psql "$@"
        '';

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;

          # run postgresql server for testing
          shellHook = ''
            export PG_ROOT=$(git rev-parse --show-toplevel)
          '';
        };
      }
    );
}
