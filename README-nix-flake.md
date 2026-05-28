# Create environment with nix flake

Requirement:
- nix

Will be install:
- `ghc`: Haskell compiler
- `stack`: Haskell build tool
- and other dependencies for development, some helper scripts

Run below command to create development environment.
`nix` builds binaries and libraries from source when no pre-built binaries are available, so it may take a while to build the environment for the first time.
Also the disk space usage will be large because of the build artifacts.
After the first time, it will be faster because of caching.

```bash
nix develop
```

If you are using direnv, it will automatically load the environment when entering the directory.

My environment works under these dependencies versions.
`stack ghc` uses the system global GHC.

```console
% stack --version
3.9.3 x86_64 hpack-0.39.1

% ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7

% stack ghc -- --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7
```

# Build Project

Build project with stack.

```bash
stack build
```

# Run example

Try example show in [README.md](../README.md).

## Run PostgreSQL

You can run PostgreSQL locally with using nix flake `flake.nix`.
PostgreSQL data and pid file will be stored in `.db` directory.
Use `psql-wrapped` command to connect local PostgreSQL.

```console
### Create, and start PostgreSQL
% haskell-lenses-db-start
The files belonging to this database system will be owned by user "bl".
This user must also own the server process.

The database cluster will be initialized with locale "en_US.UTF-8".
The default database encoding has accordingly been set to "UTF8".
The default text search configuration will be set to "english".

Data page checksums are disabled.

creating directory /home/bl/git/haskell-lenses/.db ... ok
creating subdirectories ... ok
selecting dynamic shared memory implementation ... posix
selecting default max_connections ... 100
selecting default shared_buffers ... 128MB
selecting default time zone ... Asia/Tokyo
creating configuration files ... ok
running bootstrap script ... ok
performing post-bootstrap initialization ... ok
syncing data to disk ... ok

initdb: warning: enabling "trust" authentication for local connections
You can change this by editing pg_hba.conf or using the option -A, or
--auth-local and --auth-host, the next time you run initdb.

Success. You can now start the database server using:

    /nix/store/ascalaq8smb20gm7sinpiazd1064jf9a-postgresql-14.9/bin/pg_ctl -D /home/bl/git/haskell-lenses/.db -l logfile start

waiting for server to start.... done
server started
Created db links
CREATE ROLE

### Connect to PostgreSQL
% psql-wrapped
psql (14.9)
Type "help" for help.

links=#
```

If you want to stop PostgreSQL, run below command.

```console
% haskell-lenses-db-stop
waiting for server to shut down.... done
server stopped
```

## Scratch.hs

Let's run `Scratch.hs` with `stack ghci` command.

Befor running, you need create schema to PostgreSQL.

```console
% haskell-lenses-db-start; psql-wrapped < schema.sql
The files belonging to this database system will be owned by user "bl".
This user must also own the server process.

The database cluster will be initialized with locale "en_US.UTF-8".
The default database encoding has accordingly been set to "UTF8".
The default text search configuration will be set to "english".

Data page checksums are disabled.

creating directory /home/bl/git/haskell-lenses/.db ... ok
creating subdirectories ... ok
selecting dynamic shared memory implementation ... posix
selecting default max_connections ... 100
selecting default shared_buffers ... 128MB
selecting default time zone ... Asia/Tokyo
creating configuration files ... ok
running bootstrap script ... ok
performing post-bootstrap initialization ... ok
syncing data to disk ... ok

initdb: warning: enabling "trust" authentication for local connections
You can change this by editing pg_hba.conf or using the option -A, or
--auth-local and --auth-host, the next time you run initdb.

Success. You can now start the database server using:

    /nix/store/ascalaq8smb20gm7sinpiazd1064jf9a-postgresql-14.9/bin/pg_ctl -D /home/bl/git/haskell-lenses/.db -l logfile start

waiting for server to start.... done
server started
CREATE ROLE
CREATE TABLE
CREATE TABLE
```

Then run `stack ghci` command to load `Scratch.hs` in GHCi.

```console
% stack ghci Scratch.hs
### insert data to PostgreSQL
*Scratch> test_put albums unchangedAlbums
"SELECT \"albums\".\"album\", \"albums\".\"quantity\" FROM \"albums\" WHERE TRUE"
*Scratch> test_put tracks unchangedTracks
"SELECT \"tracks\".\"track\", \"tracks\".\"date\", \"tracks\".\"rating\", \"tracks\".\"album\" FROM \"tracks\" WHERE TRUE"

### Execute example
*Scratch> test_get tracks3
"SELECT \"tracks\".\"track\", \"tracks\".\"rating\", \"tracks\".\"album\", \"albums\".\"quantity\" FROM \"tracks\", \"albums\" WHERE \"tracks\".\"album\" = \"albums\".\"album\" AND \"albums\".\"quantity\" > 2"
fromList [{ track = "Lovesong", rating = 5, album = "Paris", quantity = 4 },{ track = "Lullaby", rating = 3, album = "Show", quantity = 3 },{ track = "Trust", rating = 4, album = "Wish", quantity = 5 }]

*Scratch> test_put tracks3 examplePut
"SELECT \"tracks\".\"track\", \"tracks\".\"rating\", \"tracks\".\"album\", \"albums\".\"quantity\" FROM \"tracks\", \"albums\" WHERE \"tracks\".\"album\" = \"albums\".\"album\" AND \"albums\".\"quantity\" > 2"
"SELECT \"tracks\".\"track\", \"tracks\".\"rating\", \"tracks\".\"album\", \"albums\".\"quantity\" FROM \"tracks\", \"albums\" WHERE \"tracks\".\"album\" = \"albums\".\"album\" AND ((\"tracks\".\"track\") IN (('Lovesong'), ('Lullaby')) OR (\"tracks\".\"album\") IN (('Disintegration'), ('Show'))) AND NOT (\"albums\".\"quantity\" > 2)"
"SELECT \"tracks\".\"track\", \"tracks\".\"date\" FROM \"tracks\", \"albums\" WHERE \"tracks\".\"album\" = \"albums\".\"album\" AND (\"tracks\".\"track\") IN (('Lovesong'), ('Lullaby'), ('Trust'))"
"SELECT \"tracks\".\"track\", \"tracks\".\"date\", \"tracks\".\"rating\", \"tracks\".\"album\" FROM \"tracks\" WHERE (\"tracks\".\"track\") IN (('Lovesong'), ('Lullaby'))"
"SELECT \"albums\".\"album\", \"albums\".\"quantity\" FROM \"albums\" WHERE (\"albums\".\"album\") IN (('Disintegration'), ('Galore'), ('Show'))"
"SELECT \"tracks\".\"track\", \"tracks\".\"date\", \"tracks\".\"rating\", \"tracks\".\"album\" FROM \"tracks\" WHERE (\"tracks\".\"album\") IN (('Disintegration'), ('Galore'), ('Show'))"
"SELECT \"albums\".\"album\", \"albums\".\"quantity\" FROM \"albums\" WHERE (\"albums\".\"album\") IN (('Disintegration'))"
```
