# Emacs init

Ground-truth Emacs configuration for T. Tian.

## Installation TL;DR

```sh
curl -fsSL https://raw.githubusercontent.com/alchem0x2A/.emacs.d/master/install.sh | bash
```

Or from a local checkout:

```sh
./install.sh
```

The installer targets `~/.emacs.d` by default. Override with:

```sh
TT_EMACS_DIR=/path/to/emacs.d ./install.sh
```

If an existing `~/.emacs.d` is not already managed by this installer, it is moved to a timestamped backup before installation.

## Current state

This repo is being rebuilt as a minimal, single-file Emacs configuration. Active configuration lives in `init.el`; historical configuration files live under `archive/` with their original hierarchy preserved.

Local Custom state belongs in `~/.emacs.d/custom.el` and is intentionally not version-controlled.
