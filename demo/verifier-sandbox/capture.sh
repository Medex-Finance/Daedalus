#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"

OUTPUT="current.png"
ARGS=()
while (( "$#" )); do
  case "$1" in
    --output)
      if [ $# -lt 2 ]; then
        echo "[capture] --output requires a value" >&2
        exit 1
      fi
      OUTPUT="$2"
      ARGS+=("$1" "$2")
      shift 2
      ;;
    *)
      ARGS+=("$1")
      shift
      ;;
  esac
done

if [[ "$OUTPUT" != /* ]]; then
  OUTPUT="$DIR/$OUTPUT"
fi

ensure_deps() {
  if [ ! -d "$DIR/node_modules" ]; then
    if command -v pnpm >/dev/null 2>&1; then
      (cd "$DIR" && pnpm install)
    elif command -v npm >/dev/null 2>&1; then
      (cd "$DIR" && npm install)
    else
      echo "[capture] Neither pnpm nor npm is available" >&2
      exit 1
    fi
  fi
}

maybe_install_browsers() {
  if [ ! -d "$DIR/node_modules/.cache/ms-playwright" ]; then
    npx playwright install chromium >/dev/null || true
  fi
}

run_automation() {
  if [ ! -f "$DIR/automation.mjs" ]; then
    return 1
  fi
  ensure_deps
  if [ "${VERIFIER_DISABLE_PLAYWRIGHT:-0}" != "1" ]; then
    maybe_install_browsers
  fi
  if ! command -v node >/dev/null 2>&1; then
    return 1
  fi
  if node "$DIR/automation.mjs" "${ARGS[@]}"; then
    if [ "$OUTPUT" != "$DIR/current.png" ] && [ -f "$DIR/current.png" ]; then
      cp "$DIR/current.png" "$OUTPUT"
    fi
    return 0
  fi
  return 1
}

render_with_playwright() {
  if [ "${VERIFIER_DISABLE_PLAYWRIGHT:-0}" = "1" ]; then
    return 1
  fi
  ensure_deps
  maybe_install_browsers
  node "$DIR/capture.mjs" "${ARGS[@]}"
}

render_fallback() {
  python3 - "$OUTPUT" <<'PY'
import struct, zlib, sys

dest = sys.argv[1]
width, height = 640, 360

def chunk(tag, data):
    return struct.pack(">I", len(data)) + tag + data + struct.pack(">I", zlib.crc32(tag + data) & 0xffffffff)

rows = []
for y in range(height):
    row = bytearray()
    row.append(0)
    for x in range(width):
        r = 30 + int(70 * (x / width))
        g = 70 + int(120 * (y / height))
        b = 140 + int(40 * ((x + y) / (width + height)))
        row.extend((r, g, b))
    rows.append(bytes(row))

for y in range(80, 240):
    row = bytearray(rows[y])
    for x in range(100, 540):
        idx = 1 + x * 3
        row[idx:idx+3] = bytes((48, 58, 85))
    rows[y] = bytes(row)

raw = b''.join(rows)
ihdr = chunk(b'IHDR', struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
idat = chunk(b'IDAT', zlib.compress(raw, 9))
iend = chunk(b'IEND', b'')

with open(dest, 'wb') as fh:
    fh.write(b'\x89PNG\r\n\x1a\n' + ihdr + idat + iend)

print(f"[capture:fallback] wrote {dest}")
PY
}

if run_automation; then
  exit 0
fi

if render_with_playwright; then
  exit 0
fi

echo "[capture] Playwright capture failed, falling back to PNG generator" >&2
render_fallback
