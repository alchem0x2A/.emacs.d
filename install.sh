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

backup_path() {
  local path="$1"
  if [ -e "$path" ] || [ -L "$path" ]; then
    local backup="$path.backup.$(date +%Y%m%d-%H%M%S)"
    log "backing up existing $path to $backup"
    mv "$path" "$backup"
  fi
}

backup_shadowing_init_files() {
  # These files have higher startup precedence than $DEST/init.el and would
  # otherwise make the installed config appear to do nothing.
  backup_path "$HOME/.emacs"
  backup_path "$HOME/.emacs.el"
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

# Register the cleanup function upon exit.
cleanup() {
  if [ -n "${TMPDIR:-}" ] && [ -d "$TMPDIR" ]; then
    rm -rf "$TMPDIR"
  fi
}
trap cleanup EXIT

if same_path "$SRC" "$DEST"; then
  backup_shadowing_init_files
  log "source and destination are already $DEST"
  printf '%s\n' "$REPO_URL" > "$MARKER"
  exit 0
fi

backup_shadowing_init_files

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
  rsync -a --delete -P \
    --exclude '*~' \
    --exclude '*.elc' \
    --include '/init.el' \
    --include '/vendor/' \
    --include '/vendor/***' \
    --include '/snippets/' \
    --include '/snippets/***' \
    --exclude '*' \
    "$SRC/" "$DEST/"
else
  # Rsync not present: replace only the managed files/directories.
  rm -f "$DEST/init.el"
  rm -rf "$DEST/vendor" "$DEST/snippets"
  cp "$SRC/init.el" "$DEST/init.el"
  if [ -d "$SRC/vendor" ]; then
    mkdir -p "$DEST/vendor"
    (cd "$SRC/vendor" && tar --exclude='*~' --exclude='*.elc' -cf - .) | (cd "$DEST/vendor" && tar -xf -)
  fi
  if [ -d "$SRC/snippets" ]; then
    mkdir -p "$DEST/snippets"
    (cd "$SRC/snippets" && tar --exclude='*~' --exclude='*.elc' -cf - .) | (cd "$DEST/snippets" && tar -xf -)
  fi
fi

printf '%s\n' "$REPO_URL" > "$MARKER"
log "done"
