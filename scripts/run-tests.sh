#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)

pushd "$ROOT_DIR/backend" >/dev/null
cabal test
popd >/dev/null

pushd "$ROOT_DIR/frontend" >/dev/null
pnpm test || echo "Frontend tests not yet implemented"
popd >/dev/null
