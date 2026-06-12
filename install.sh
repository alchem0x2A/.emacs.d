#!/usr/bin/env bash
set -euo pipefail

# Install from the default repo
REPO_URL="${TT_EMACS_REPO_URL:-https://github.com/alchem0x2A/.emacs.d.git}"
DEST="${TT_EMACS_DIR:-$HOME/.emacs.d}"
# A marker file will be written to mark where the download file is from
MARKER="$DEST/.tt-emacs-install-source"

log() { printf '[tt-emacs] %s\n' "$*"; }

same_path() {
  [ -d "$1" ] && [ -d "$2" ] && [ "$(cd "$1" && pwd -P)" = "$(cd "$2" && pwd -P)" ]
}

# Locate source tree. If this script is piped through curl, clone the repo.
if [ -n "${BASH_SOURCE[0]:-}" ] && [ -f "${BASH_SOURCE[0]}" ]; then
  SRC="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
  TMPDIR=""
else
  TMPDIR="$(mktemp -d)"
  SRC="$TMPDIR/repo"
  log "cloning $REPO_URL"
  git clone --depth 1 "$REPO_URL" "$SRC"
fi

cleanup() {
  if [ -n "${TMPDIR:-}" ] && [ -d "$TMPDIR" ]; then
    rm -rf "$TMPDIR"
  fi
}
trap cleanup EXIT

if same_path "$SRC" "$DEST"; then
  log "source and destination are already $DEST"
  printf '%s\n' "$REPO_URL" > "$MARKER"
  exit 0
fi

# If the marker does not exist, create a backup. Otherwise the repo is already tracked
# and any files can be overwritten.
if [ -e "$DEST" ]; then
  if [ -f "$MARKER" ] && [ "$(cat "$MARKER")" = "$REPO_URL" ]; then
    log "updating existing managed $DEST"
  else
    BACKUP="$DEST.backup.$(date +%Y%m%d-%H%M%S)"
    log "backing up existing $DEST to $BACKUP"
    mv "$DEST" "$BACKUP"
    mkdir -p "$DEST"
  fi
else
  mkdir -p "$DEST"
fi

log "installing files to $DEST"
if command -v rsync >/dev/null 2>&1; then
  rsync -a --delete \
    --exclude '.git/' \
    --exclude 'elpa/' \
    --exclude '*.elc' \
    "$SRC/" "$DEST/"
else
  find "$DEST" -mindepth 1 -maxdepth 1 ! -name elpa -exec rm -rf {} +
  (cd "$SRC" && tar --exclude='./.git' --exclude='./elpa' --exclude='*.elc' -cf - .) | (cd "$DEST" && tar -xf -)
fi

printf '%s\n' "$REPO_URL" > "$MARKER"
log "done"
