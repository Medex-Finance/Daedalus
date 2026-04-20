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
        haskellPackages = pkgs.haskell.packages.ghc984;
        frontendPkg = pkgs.stdenv.mkDerivation {
          pname = "orchestrator-frontend";
          version = "0.1.0";
          src = ./frontend;
          buildInputs = [ pkgs.nodejs_20 pkgs.pnpm pkgs.elmPackages.elm ];
          buildPhase = ''
            export NODE_OPTIONS=--openssl-legacy-provider
            export ELM_BINARY=${pkgs.elmPackages.elm}/bin/elm
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
          frontend = frontendPkg;
          default = frontendPkg;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.git
            pkgs.nodejs_20
            pkgs.pnpm
            pkgs.elmPackages.elm
            pkgs.sqlite
            pkgs.zlib
            pkgs.zlib.dev
            pkgs.watchman
            pkgs.cabal-install
            haskellPackages.ghc
            haskellPackages.haskell-language-server
            haskellPackages.ormolu
            haskellPackages.aeson-typescript
            haskellPackages.fast-logger
            # Playwright runtime deps + managed browsers
            pkgs.playwright-driver.browsers
            pkgs.chromium
            pkgs.glib
            pkgs.nspr
            pkgs.nss
            pkgs.dbus
            pkgs.gtk3
            pkgs.at-spi2-core
            pkgs.mesa
            pkgs.alsa-lib
            pkgs.libdrm
            pkgs.udev
            pkgs.libxkbcommon
            pkgs.libx11
            pkgs.libxcomposite
            pkgs.libxdamage
            pkgs.libxext
            pkgs.libxfixes
            pkgs.libxrandr
            pkgs.libxcb
          ];
          shellHook = ''
            export CABAL_DIR=$PWD/dist-newstyle
            export PATH=$PWD/node_modules/.bin:$PATH
            export ELM_BINARY=${pkgs.elmPackages.elm}/bin/elm
            export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
            export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
            export CHROMIUM_PATH=$(find ${pkgs.playwright-driver.browsers} -path '*chrome-linux/chrome' -type f -print -quit)
            export PLAYWRIGHT_LAUNCH_OPTIONS_EXECUTABLE_PATH="$CHROMIUM_PATH"
            echo "Dev shell ready: run cabal build && pnpm dev"
          '';
        };
      }
    );
}
