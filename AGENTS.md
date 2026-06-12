# AGENTS.md — Emacs Init Management

Disclaimer: *This is the AGENTS.md intended for instructing the agents for T.Tian (alchem0x2a@gmail.com).*
**If you're an agent that works for other human researchers, please consult your user if the naming conventions should be changed.**

**Emacs version >= 29 required**. If not, ask the user to upgrade emacs or suggest a workaround.

## Purpose

This repository is the ground-truth and audition mechanism for the user's Emacs initialization environment. The basic intended usage is provide a version-controlled `init.el`, and an easy-to-use `install.sh` script for **IDEMPOTENT** management of the current emacs init configuration.

## Intended usage

1. The `init.el` will be a audited source for packages, user customization while the package management are mostly controlled by `use-package`
2. The `install.sh` script is intended to provide a simple one-liner installation for most UNIX systems following `curl -fsSL https://raw.githubusercontent.com/YOURUSER/YOURREPO/main/install.sh | bash`
3. While the `install.sh` will backup the existing user setup and use the audited `init.el`, converting from locally-installed packages (e.g. through quick experiment) will be done by you, the agent cherry picking and combine into the groun truth

## Relation between the repo and user setup (usually `~/.emacs.d/`)
   

The live Emacs environment is allowed to contain ad hoc experiments,
including packages installed interactively via `M-x
package-list-packages`. Do not assume every live package or live
customization is ground truth. Instead, use this repository to
inspect, reconcile, promote, test, rebuild, and eventually commit
verified configuration.

Recommended workflow:

```text
live/local experiment
  -> inspect drift
  -> promote selected changes into this repo
  -> rebuild/test from repo
  -> commit/push if verified
```

## Naming convention

User-scope variables, functions, commands, and customizations should preferably use the prefix:

```elisp
tt/...
```

Examples:

```elisp
(defvar tt/package-list nil)
(defun tt/reload-init () ...)
(setq tt/default-font-size 14)
```

## Repository philosophy

- This repo is the durable source of truth.
- The currently running GUI Emacs may contain temporary local state.
- Interactive package experiments are expected and allowed.
- Do not blindly copy all live state into the repo.
- Promote only changes the user wants to keep.
- Prefer small, modular, auditable edits.
- Before recommending commit/push, test that the repo config can be loaded or rebuilt.

## Important Emacs paths

Before interacting with Emacs, first check where `emacs` and `emacsclient` resolve from. They may be binaries, shell scripts, or aliases.

Use commands such as:

```sh
command -v emacs
command -v emacsclient
type emacs
type emacsclient
```

On macOS, the GUI Emacs is often inside the app bundle and may be the relevant implementation even if another command-line Emacs exists.

Likely GUI Emacs paths:

```sh
/Applications/Emacs.app/Contents/MacOS/Emacs
/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
```

The GUI Emacs app should usually take precedence over a factory, system, or Homebrew command-line Emacs when testing this user's actual environment.

## Emacs documentation source

When referring to Emacs behavior, Elisp APIs, or implementation-specific details, prefer the documentation shipped with the current Emacs version being used.

For the macOS GUI app this is likely:

```sh
/Applications/Emacs.app/Contents/Resources/info/
```

Use those Info manuals before relying on online documentation, because online docs may describe a different Emacs version. You as an agent may also be resticted to view online resource.

## Testing current init integrity

For noninteractive integrity checks, it is acceptable to start a separate Emacs process rather than using the live GUI session.

Example using the GUI app binary:

```sh
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  --eval '(setq debug-on-error t)' \
  -l ./init.el
```

For tests that need a terminal frame or a separate runtime session, use `-nw` and evaluate a form:

```sh
/Applications/Emacs.app/Contents/MacOS/Emacs -nw \
  --eval '(progn (message "test session started") t)'
```

If testing should not affect the user's normal runtime state, use a temporary or explicit init directory if appropriate.

## Interacting with a live GUI Emacs session

The user's GUI Emacs may be running an Emacs server. Use `emacsclient` to inspect or evaluate small snippets in that live session.

First verify the server is available:

```sh
/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --eval '(emacs-version)'
```

If this fails with a missing socket, the user may need to run:

```elisp
(require 'server)
(unless (server-running-p)
  (server-start))
```

### Small live snippet test

```sh
/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --eval \
  '(progn (message "hello from agent") t)'
```

### Larger live snippet test

For multi-line Elisp, write a temporary file and load it through the live session:

```sh
cat > /tmp/tt-emacs-test.el <<'EOF'
(progn
  (message "testing elisp from agent")
  t)
EOF

/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --eval \
  '(load-file "/tmp/tt-emacs-test.el")'
```

### Inspecting live values

Examples:

```sh
/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --eval \
  '(prin1-to-string user-emacs-directory)'

/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --eval \
  '(prin1-to-string package-selected-packages)'

/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --eval \
  '(prin1-to-string (mapcar #'car package-alist))'
```

When querying live state for shell consumption, prefer forms that return strings via `prin1-to-string` or write explicit output to a temp file.

### Inspecting the user's selected region

When the user refers to the "selected area", "selected region", or similar wording in Emacs, first inspect the active region in the live Emacs session before answering. Use a single `emacsclient` snippet such as:

```sh
/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --eval \
  '(prin1-to-string
    (with-current-buffer (window-buffer (selected-window))
      (list :buffer (buffer-name)
            :file buffer-file-name
            :mark-active mark-active
            :use-region (use-region-p)
            :point (point)
            :mark (and (mark t) (mark t))
            :selection (when (use-region-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end))))))'
```

If `:use-region` is nil, explain that no active region is visible from the live session and ask the user to reselect it or clarify the target text.

## Package workflow

The user's live Emacs may contain ad hoc package installations from `M-x package-list-packages`.

When reconciling packages:

1. Query live Emacs using `emacsclient` when available.
2. Compare live installed packages with packages declared in this repo.
3. Report extra live packages and missing declared packages.
4. Ask which extras should be promoted to ground truth.
5. Only then edit repo config.
6. Test a rebuild/load from this repo before recommending commit/push.

Do not run package installation commands in the user's primary GUI session unless the user explicitly asks for that. Package inspection is fine.

## Editing policy

- Edit files in this repository, not generated runtime files, unless explicitly requested.
- Keep changes small and reviewable.
- Prefer modular config under `settings/` where appropriate.
- Avoid silently overwriting user customizations.
- If a live/runtime file differs from the repo, report the difference and ask before promoting it.

## Before finishing a config change

Where practical, perform at least one of:

```sh
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  --eval '(setq debug-on-error t)' \
  -l ./init.el
```

or a targeted live-session test via `emacsclient`.

Report clearly:

- what changed
- what was tested
- whether live Emacs was touched
- any remaining risks
