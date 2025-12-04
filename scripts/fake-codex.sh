#!/usr/bin/env bash
set -euo pipefail

# Simple Codex CLI stub used in tests. It accepts the same invocation
# pattern as `codex exec … -`, consumes stdin, and prints a short summary.

if [[ "$#" -gt 0 && "$1" == "exec" ]]; then
  shift
fi

# Swallow stdin (the orchestration prompt).
cat >/dev/null || true

echo "[fake-codex] run completed successfully"
exit 0
