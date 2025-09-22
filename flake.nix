{
  description = "Multi-agent project manager platform";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        haskellPackages = pkgs.haskellPackages;
        backendPkg = haskellPackages.callCabal2nix "orchestrator-backend" ./backend { };
        frontendPkg = pkgs.stdenv.mkDerivation {
          pname = "orchestrator-frontend";
          version = "0.1.0";
          src = ./frontend;
          buildInputs = [ pkgs.nodejs_20 pkgs.pnpm ];
          buildPhase = ''
            export NODE_OPTIONS=--openssl-legacy-provider
            pnpm install --frozen-lockfile || pnpm install
            pnpm build
          '';
          installPhase = ''
            mkdir -p $out
            cp -r dist $out/
          '';
        };
      in
      {
        packages = {
          backend = backendPkg;
          frontend = frontendPkg;
          default = backendPkg;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.git
            pkgs.nodejs_20
            pkgs.pnpm
            pkgs.sqlite
            pkgs.zlib
            pkgs.zlib.dev
            pkgs.watchman
            pkgs.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.ormolu
            pkgs.haskellPackages.aeson-typescript
            pkgs.haskellPackages.fast-logger
          ];
          shellHook = ''
            export CABAL_DIR=$PWD/dist-newstyle
            export PATH=$PWD/node_modules/.bin:$PATH
            echo "Dev shell ready: run cabal build && pnpm dev"
          '';
        };
      }
    );
}
