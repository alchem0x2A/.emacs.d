# Emacs init

Ground-truth Emacs configuration for T. Tian.

## Installation TL;DR

```sh
curl -fsSL https://raw.githubusercontent.com/alchem0x2A/.emacs.d/main/install.sh | bash
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

This repo has been reset to a minimal bootstrap state. `init.el` is intentionally empty for now; new configuration should be rebuilt incrementally and kept auditable.

Historical configuration files were moved under `archive/` with their original hierarchy preserved, including the old `README.org`.
