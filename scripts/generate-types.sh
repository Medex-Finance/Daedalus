#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT_DIR/backend"

cabal run generate-types -- "$ROOT_DIR/frontend/src/types/app.d.ts" "$ROOT_DIR/frontend/src/Generated/Api.elm"
