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
            pkgs.xorg.libX11
            pkgs.xorg.libXcomposite
            pkgs.xorg.libXdamage
            pkgs.xorg.libXext
            pkgs.xorg.libXfixes
            pkgs.xorg.libXrandr
            pkgs.xorg.libxcb
          ];
          shellHook = ''
            export CABAL_DIR=$PWD/dist-newstyle
            export PATH=$PWD/node_modules/.bin:$PATH
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
