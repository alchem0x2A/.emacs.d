;;; init.el --- T. Tian's Emacs init -*- lexical-binding: t; -*-

;;  TT's Emacs Initialization File:
;; Single-file Emacs configuration, rebuilt incrementally.
;; Section headers use three semicolons so `outline-minor-mode' can fold them.
;; Use `tt/toggle-outline' to collapse the headings!
;;
;; Use `curl -fsSL https://raw.githubusercontent.com/alchem0x2A/.emacs.d/master/install.sh | bash' to install / update the current runner

;;;
;;; 1. Universal pre-start setup

;; This setup requires Emacs > 29.1 as the `treesitter' support
;; is needed

;;;; 1.1 Setup for package system & melpa

(when (version< emacs-version "29.1")
  (error "This init requires Emacs 29.1 or newer; current version is %s"
         emacs-version))

;; the use-short-answers to make yes / no accept y-or-no-p 
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      use-short-answers t)

;; The `package' package itself should be installed
;; For Emacs 29.x, we may need to make sure that the vc-use-package
;; is installed first.



(require 'cl-lib)
(require 'package-vc)
(require 'package)

(unless (package-installed-p 'vc-use-package)
  ;; The vc-use-package package itself must be declared BEFORE loading
  ;; use-package to make the :vc keyword available on Emacs 29.x.
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(require 'use-package)
(require 'outline)
(require 'seq)


;; GMT+8: prefer TUNA mirrors for package bootstrap reliability
;; Otherwise the default gnu and melpa

(setq package-archives
      (if (= (car (current-time-zone)) (* 8 60 60))
          '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
            ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
            ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/"))))

;; Make sure the packages defined in the init file are always
;; always ensured
(eval-and-compile
    (setq use-package-always-ensure t
          use-package-expand-minimally t))

(package-initialize)

;;; 2 System-wide variables and functions (`tt/' namespace)

;;;; 2.1 tt/ namaspace variables
(defconst tt/macos-p (eq system-type 'darwin)
  "Non-nil when running on macOS / Darwin.")

(defconst tt/linux-p (eq system-type 'gnu/linux)
  "Non-nil when running on GNU/Linux.")

(defconst tt/ns-p (eq window-system 'ns)
  "Non-nil when running the macOS NS GUI port.")

(defconst tt/macos-command-is-super-p
  (and tt/ns-p
       (boundp 'ns-command-modifier)
       (eq ns-command-modifier 'super))
  "Non-nil when macOS Command currently generates Emacs Super events.")

(defconst tt/init-repo-owner
  "alchem0x2A"
  "GitHub owner of the repository hosting this Emacs config.")

(defconst tt/init-repo-name
  ".emacs.d"
  "GitHub repository name hosting this Emacs config.")

(defconst tt/init-repo-branch
  "master"
  "Git branch used by the installer for this Emacs config.")

(defconst tt/install-url
  (format "https://raw.githubusercontent.com/%s/%s/%s/install.sh"
          tt/init-repo-owner
          tt/init-repo-name
          tt/init-repo-branch)
  "URL of the installer used to install or update this Emacs config.")

(defconst tt/vendor-dir
  (expand-file-name "vendor" (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to vendored third-party Elisp.")

(defconst tt/snippets-dir
  (expand-file-name "snippets" (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to repo-managed Yasnippet snippets.")


;;;; 2.2 tt/ namespace defuns

(defun tt/select-current-line ()
  "Select the current line from beginning to end."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun tt/ensure-vendor-and-load (name)
  "Ensure vendored package NAME exists and add it to `load-path'."
  (let ((dir (expand-file-name name tt/vendor-dir)))
    (unless (file-directory-p dir)
      (error "Missing vendor dir: %s" dir))
    (add-to-list 'load-path dir)))


(defun tt/treesit-auto-recipe-for (lang)
  "Return the `treesit-auto' recipe for LANG, or nil."
  (seq-find (lambda (recipe)
              (eq (treesit-auto-recipe-lang recipe) lang))
            treesit-auto-recipe-list))

(defun tt/treesit-auto-apply-abi-revisions ()
  "Apply TT's tree-sitter grammar revision pins for this Emacs ABI."
  (when (eq tt/treesit-abi-version 14)
    (dolist (entry tt/treesit-abi14-revisions)
      (when-let ((recipe (tt/treesit-auto-recipe-for (car entry))))
        (setf (aref recipe 7) (cdr entry)))))
  (when (and tt/treesit-abi-version
             (>= tt/treesit-abi-version 15))
    (dolist (entry tt/treesit-abi15-revisions)
      (when-let ((recipe (tt/treesit-auto-recipe-for (car entry))))
        (setf (aref recipe 6) (cdr entry))))))

(defun tt/backward-delete-line ()
  "Delete from point to the beginning of the current line."
  (interactive)
  (delete-region (point) (line-beginning-position)))

(defun tt/forward-delete-line ()
  "Delete from point to the end of the current line."
  (interactive)
  (delete-region (point) (line-end-position)))

(defun tt/forward-delete-word-or-subword (&optional arg)
  "Detel a word from the synatx-subword package"
  (interactive "p")
  (if (bound-and-true-p syntax-subword-mode)
      (delete-region (point) (save-excursion
                               (syntax-subword-forward arg)
                               (point)))
    (delete-region (point) (save-excursion
                             (subword-forward arg)
                             (point)))))

(defun tt/backward-delete-word-or-subword (&optional arg)
  (interactive "p")
  (tt/forward-delete-word-or-subword (- arg)))



(defun tt/toggle-outline ()
  "Toggle `outline-minor-mode' for the current buffer.
 When enabling it, configure cycling/buttons/highlighting locally and
collapse to top-level headings.  When disabling it, leave the buffer
fully visible."
  (interactive)
  (if (bound-and-true-p outline-minor-mode)
      (progn
        (outline-show-all)
        (outline-minor-mode 'toggle))
    
    (setq-local outline-minor-mode-cycle t
                outline-minor-mode-use-buttons t
                outline-minor-mode-highlight t)
    (outline-minor-mode 'toggle)
    (outline-show-all)
    (outline-hide-sublevels 2)))

(defun tt/update-packages ()
  "Refresh package archives and upgrade installed ELPA and VC packages."
  (interactive)
  (when (y-or-n-p "Refresh package archives and upgrade packages? ")
    (package-refresh-contents)

    ;; Regular ELPA/MELPA/etc. packages.
    (if (fboundp 'package-upgrade-all)
        (package-upgrade-all)
      (message "`package-upgrade-all' is unavailable in this Emacs."))

    ;; Git/VC packages installed through package-vc / vc-use-package.
    (if (fboundp 'package-vc-upgrade-all)
        (package-vc-upgrade-all)
      (message "`package-vc-upgrade-all' is unavailable in this Emacs."))

    (message "Package update finished.")))



(defun tt/update-init ()
  "Update this Emacs config by running the remote installer script.

After the install finishes, reload with `M-x load-file' or restart Emacs."
  (interactive)
  (let ((cmd (format "curl -fsSL %s | bash" (shell-quote-argument tt/install-url))))
    (when (y-or-n-p (format "Run this installer command? %s " cmd))
      (async-shell-command cmd "*tt/update-init*")
      (message "Init files synced. Reload with `M-x load-file' or restart Emacs.")
      )
    )
  )

;;;; 2.3 Emacs-local micromamba environment

(defconst tt/micromamba-dir
  (expand-file-name "micromamba" user-emacs-directory)
  "Directory for TT's managed micromamba executable.")

(defconst tt/micromamba-managed-executable
  (expand-file-name "bin/micromamba" tt/micromamba-dir)
  "Path to TT's managed micromamba executable.")

(defconst tt/conda-env-prefix
  (expand-file-name "conda-env" user-emacs-directory)
  "Prefix for TT's Emacs-local micromamba environment.")

(defconst tt/conda-env-bin-dir
  (expand-file-name "bin" tt/conda-env-prefix)
  "Bin directory for TT's Emacs-local micromamba environment.")

(defconst tt/conda-env-package-alist
  '((core
     :packages ("python" "pip" "uv" "ruff" "nodejs" "ripgrep" "fd-find")
     :tools ("python" "pip" "uv" "ruff" "node" "npm" "rg" "fd"))
    (python-ide
     :packages ("ty" "ruff")
     :tools ("ty" "ruff")))
  "Package/tool groups for TT's Emacs-local micromamba environment.")

(defun tt/conda-env-group-plist (group)
  "Return package GROUP plist from `tt/conda-env-package-alist'."
  (or (alist-get group tt/conda-env-package-alist)
      (error "Unknown conda env package group: %s" group)))

(defun tt/conda-env-group-packages (group)
  "Return conda-forge packages for GROUP."
  (plist-get (tt/conda-env-group-plist group) :packages))

(defun tt/conda-env-group-tools (group)
  "Return executable names expected for GROUP."
  (plist-get (tt/conda-env-group-plist group) :tools))

(defun tt/conda-env-packages (&rest groups)
  "Return de-duplicated conda-forge packages for GROUPS."
  (delete-dups (apply #'append (mapcar #'tt/conda-env-group-packages groups))))

(defun tt/conda-env-tools (&rest groups)
  "Return de-duplicated executable names expected for GROUPS."
  (delete-dups (apply #'append (mapcar #'tt/conda-env-group-tools groups))))

(defun tt/conda-env-add-to-path ()
  "Add TT's Emacs-local micromamba environment to `exec-path' and PATH.

This is harmless when the environment does not exist yet."
  (add-to-list 'exec-path tt/conda-env-bin-dir)
  (let* ((path (or (getenv "PATH") ""))
         (parts (split-string path path-separator t)))
    (unless (member tt/conda-env-bin-dir parts)
      (setenv "PATH" (concat tt/conda-env-bin-dir path-separator path)))))

(defun tt/micromamba-platform ()
  "Return the micromamba download platform for this system."
  (pcase (list system-type system-configuration)
    (`(darwin ,config) (if (string-match-p "aarch64\\|arm64" config) "osx-arm64" "osx-64"))
    (`(gnu/linux ,config) (if (string-match-p "aarch64\\|arm64" config) "linux-aarch64" "linux-64"))
    (_ (error "Unsupported micromamba platform: %s %s" system-type system-configuration))))

(defun tt/micromamba-url ()
  "Return the official latest micromamba download URL for this system."
  (format "https://micro.mamba.pm/api/micromamba/%s/latest" (tt/micromamba-platform)))

(defun tt/micromamba-executable ()
  "Return system micromamba or TT's managed micromamba, or nil."
  (or (executable-find "micromamba")
      (when (file-executable-p tt/micromamba-managed-executable)
        tt/micromamba-managed-executable)))

(defun tt/micromamba-install-command ()
  "Build the command that installs micromamba under `tt/micromamba-dir'."
  (let ((url (tt/micromamba-url))
        (dir (shell-quote-argument tt/micromamba-dir)))
    (format (concat "tmp=$(mktemp -d) && "
                    "mkdir -p %s/bin && "
                    "curl -LfsS %s -o $tmp/micromamba.tar.bz2 && "
                    "tar -xjf $tmp/micromamba.tar.bz2 -C $tmp bin/micromamba && "
                    "cp $tmp/bin/micromamba %s/bin/micromamba && "
                    "chmod +x %s/bin/micromamba && "
                    "rm -rf $tmp")
            dir (shell-quote-argument url) dir dir)))

(defun tt/micromamba-install ()
  "Install TT's managed micromamba executable after confirmation."
  (interactive)
  (if (tt/micromamba-executable)
      (message "micromamba already available: %s" (tt/micromamba-executable))
    (tt/conda-env-run (tt/micromamba-install-command))))

(defun tt/conda-env-exists-p ()
  "Non-nil when TT's Emacs-local micromamba environment exists."
  (file-directory-p tt/conda-env-bin-dir))

(defun tt/conda-env-executable (name)
  "Return executable NAME from TT's env, falling back to `exec-path'."
  (let ((local (expand-file-name name tt/conda-env-bin-dir)))
    (or (when (file-executable-p local) local)
        (executable-find name))))

(defun tt/conda-env-command (action packages)
  "Build a micromamba ACTION command for PACKAGES using conda-forge."
  (let ((micromamba (tt/micromamba-executable)))
    (unless micromamba
      (error "No micromamba executable found; run M-x tt/micromamba-install"))
    (mapconcat #'identity
               (append (list (shell-quote-argument micromamba)
                             action
                             "-y"
                             "-p"
                             (shell-quote-argument tt/conda-env-prefix)
                             "--override-channels"
                             "-c"
                             "conda-forge")
                       (mapcar #'shell-quote-argument packages))
               " ")))

(defun tt/conda-env-run (command)
  "Ask before running COMMAND asynchronously."
  (when (y-or-n-p (format "Run command? %s " command))
    (async-shell-command command "*tt/conda-env*")
    (message "Started micromamba env command in *tt/conda-env*.")))

(defun tt/conda-env-create (&optional recreate)
  "Create TT's Emacs-local micromamba environment.

With prefix argument RECREATE, offer to delete and recreate the existing env."
  (interactive "P")
  (cond
   ((and (tt/conda-env-exists-p)
         (not recreate))
    (message "Micromamba env already exists at %s" tt/conda-env-prefix))
   ((and (tt/conda-env-exists-p)
         recreate
         (y-or-n-p (format "Delete and recreate %s? " tt/conda-env-prefix)))
    (delete-directory tt/conda-env-prefix t)
    (tt/conda-env-run (tt/conda-env-command "create" (tt/conda-env-packages 'core))))
   ((not (tt/conda-env-exists-p))
    (tt/conda-env-run (tt/conda-env-command "create" (tt/conda-env-packages 'core))))))

(defun tt/conda-env-install-core ()
  "Install or update TT's core Emacs-local micromamba packages."
  (interactive)
  (tt/conda-env-run (tt/conda-env-command "install" (tt/conda-env-packages 'core))))

(defun tt/conda-env-missing-core-tools ()
  "Return core tool executables missing from `exec-path'."
  (seq-remove #'executable-find (tt/conda-env-tools 'core)))

(defun tt/conda-env-report ()
  "Report TT's Emacs-local micromamba environment status."
  (interactive)
  (let ((missing (tt/conda-env-missing-core-tools)))
    (message "micromamba=%s env=%s missing=%s"
             (or (tt/micromamba-executable) "missing")
             (if (tt/conda-env-exists-p) "present" "missing")
             (if missing (mapconcat #'identity missing ", ") "none"))))

(defun tt/python-ide-missing-tools ()
  "Return Python IDE tool executables missing from `exec-path'."
  (seq-remove #'executable-find (tt/conda-env-tools 'python-ide)))

(defun tt/python-ide-install-tools ()
  "Install TT's Python IDE tools into the Emacs-local micromamba env."
  (interactive)
  (cond
   ((not (tt/micromamba-executable))
    (message "No micromamba found. Run M-x tt/micromamba-install first."))
   ((not (tt/conda-env-exists-p))
    (when (y-or-n-p "Emacs-local env is missing. Create it with Python IDE tools? ")
      (tt/conda-env-run
       (tt/conda-env-command "create"
                             (tt/conda-env-packages 'core 'python-ide)))))
   (t
    (tt/conda-env-run (tt/conda-env-command "install" (tt/conda-env-packages 'python-ide))))))

(defconst tt/python-ide-local-env-names
  '(".venv" "venv" ".env" "env")
  "Project-local Python environment directory names recognized by TT.")

(defun tt/python-ide-project-root ()
  "Return a conservative project root for Python interpreter discovery."
  (or (locate-dominating-file default-directory "pyproject.toml")
      (locate-dominating-file default-directory "setup.py")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun tt/python-ide-env-bin-dir (env-dir)
  "Return ENV-DIR's executable directory."
  (expand-file-name (if (eq system-type 'windows-nt) "Scripts" "bin") env-dir))

(defun tt/python-ide-interpreter-in-env (env-dir)
  "Return the Python executable in ENV-DIR, or nil."
  (let ((python (expand-file-name (if (eq system-type 'windows-nt) "python.exe" "python")
                                  (tt/python-ide-env-bin-dir env-dir))))
    (when (file-executable-p python)
      python)))

(defun tt/python-ide-current-interpreter ()
  "Return the best Python interpreter for the current buffer.

This intentionally treats conda, virtualenv, and uv-created environments
equally: an environment is just a prefix containing a Python executable."
  (or (when-let ((prefix (getenv "CONDA_PREFIX")))
        (tt/python-ide-interpreter-in-env prefix))
      (when-let ((prefix (getenv "VIRTUAL_ENV")))
        (tt/python-ide-interpreter-in-env prefix))
      (let ((root (file-name-as-directory (tt/python-ide-project-root))))
        (seq-some (lambda (name)
                    (tt/python-ide-interpreter-in-env (expand-file-name name root)))
                  tt/python-ide-local-env-names))
      (executable-find "python3")
      (executable-find "python")))

(defun tt/python-ide-report ()
  "Report Python IDE tool and interpreter availability."
  (interactive)
  (let ((missing (tt/python-ide-missing-tools))
        (python (tt/python-ide-current-interpreter)))
    (message "Python IDE: python=%s ty=%s ruff=%s missing=%s"
             (or python "missing")
             (or (tt/conda-env-executable "ty") "missing")
             (or (tt/conda-env-executable "ruff") "missing")
             (if missing (mapconcat #'identity missing ", ") "none"))))


;;; 3. Global setup and keybindings

;; Use `emacs' as the one-stop place for built-in setup and
;; keybindings.

(use-package emacs
  :config
  (tt/conda-env-add-to-path)
  (unless (tt/conda-env-exists-p)
    (message "Emacs-local micromamba env is missing. Run M-x tt/conda-env-create when needed."))
  (unless (tt/micromamba-executable)
    (message "No micromamba executable found. Run M-x tt/micromamba-install if an Emacs-local env is needed."))

  (global-set-key (kbd "C-c t l") #'tt/select-current-line)
  (global-set-key (kbd "C-c t z") #'global-text-scale-adjust)
  (global-set-key (kbd "C-c t u") #'tt/update-packages)
  (global-set-key (kbd "C-c t U") #'tt/update-init)
  
  (when tt/macos-command-is-super-p
    ;; The NS port maps Command through `ns-command-modifier'.  When
    ;; it is `super', Command bindings are ordinary Emacs `s-'
    ;; bindings.
    (global-set-key (kbd "s-<backspace>") #'tt/backward-delete-line)
    (global-set-key (kbd "s-<kp-delete>") #'tt/forward-delete-line)
    (global-set-key (kbd "s-/") #'comment-dwim)
    (global-set-key (kbd "s-]") #'indent-rigidly)
    (global-set-key (kbd "s-[") #'indent-rigidly)
    (global-set-key (kbd "s-<up>") #'beginning-of-buffer)
    (global-set-key (kbd "s-<down>") #'end-of-buffer)
    (global-set-key (kbd "s-l") #'tt/select-current-line)
    (global-set-key (kbd "s-=") #'global-text-scale-adjust)
    (global-set-key (kbd "s-+") #'global-text-scale-adjust)
    (global-set-key (kbd "s--") #'global-text-scale-adjust)
    (global-set-key (kbd "s-0") #'global-text-scale-adjust)
    (global-set-key (kbd "s-p") nil)
    (global-set-key (kbd "s-q") nil)
    (global-set-key (kbd "s-o") nil)
    (global-set-key (kbd "s-y") nil))
;;;; 3.1 Global appearance setup in `use-package emacs'
  ;; Appearance power-up. Some variables to setup
  (setq
   inhibit-startup-screen t
   visible-bell t
   ring-bell-function #'ignore
   line-move-visual nil
   )

  ;; Some default setup 
  (setq-default
   fill-column 72    ;; slightly wider fill-column
   ) 

  ;; Sequential interactive setup commands these modes are not worth calling
  ;; use-package for individual setup as default are good enough

  (menu-bar-mode -1)
  (size-indication-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  (global-display-line-numbers-mode t)
  (show-paren-mode t)
  (electric-pair-mode t)
  (global-hl-line-mode t) ;; The lines are always highlighted
  (save-place-mode t)
  (savehist-mode t)
  (recentf-mode t)
  (context-menu-mode t)  ;; Works for macos, if toolbar disabled
  (global-auto-revert-mode t) ;; always use s-U revert
  (winner-mode t)

  ;; Default font size to be 150 weights
  (set-face-attribute 'default nil :height 150)

  ;; Make sure default and initial frames use maximized
  ;; full-screen appearance, while disable scroolbar
  (dolist (frame-params '((fullscreen . maximized)
                          (tool-bar-lines . 0)
                          (vertical-scroll-bars . nil)))
    (add-to-list 'default-frame-alist frame-params)
    (add-to-list 'initial-frame-alist frame-params))

)

;; Use windmove to activate multiple window movement
(use-package windmove
  :ensure nil
  :config
  (when tt/macos-command-is-super-p
    (global-set-key (kbd "M-s-<up>") #'windmove-up)
    (global-set-key (kbd "M-s-<down>") #'windmove-down)
    (global-set-key (kbd "M-s-<left>") #'windmove-left)
    (global-set-key (kbd "M-s-<right>") #'windmove-right)))

;;;; 3.2 Editing, snippets, and version control packages

(use-package magit
  :commands (magit-status)
  :bind (("C-M-g" . magit-status)))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-global-mode yas-expand yas-new-snippet yas-visit-snippet-file)
  :config
  (setq yas-snippet-dirs (list tt/snippets-dir))
  (yas-global-mode 1))

;;;; 3.3 Vendored packages for editing / appearance (light-weight)

;; fake text
(use-package lorem-ipsum
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "lorem-ipsum"))

;; respect subword hyphen etc
(use-package syntax-subword
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "syntax-subword")
  :config
  (global-syntax-subword-mode 1)
  (global-set-key (kbd "M-<backspace>") #'tt/backward-delete-word-or-subword)
  (global-set-key (kbd "M-<delete>") #'tt/backward-delete-word-or-subword))

(use-package undo-tree
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "undo-tree")
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (global-set-key (kbd "C-x u") #'undo-tree-visualize)
  (when tt/macos-command-is-super-p
    (global-set-key (kbd "s-z") #'undo-tree-undo)
    (global-set-key (kbd "s-Z") #'undo-tree-redo)))

(use-package move-text
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "move-text")
  :config
  (move-text-default-bindings))

;; Packages for hiding & simplify mode lines
(use-package diminish
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "diminish"))

;; Packages for hiding & simplify mode lines
(use-package delight
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "delight"))

;; Similar to fill-column but on a visual face
(use-package visual-fill-column
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "visual-fill-column")
  :diminish visual-fill-column-mode
  :custom
  (visual-fill-column-width 88)
  :config
  (global-visual-fill-column-mode 1))

;; Toggle visibility of unwanted ephemeral buffers

(use-package popper
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "popper")
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

;;;; 3.3 Special editing: multiple cursors
;; The multiple cursors functionalities are adapted from
;; vendored package, but allowed using own key bindings

(defun tt/mc-toggle-cursor-on-click (event)
  "Add or remove a fake cursor at mouse EVENT without enabling MC mode."
  (interactive "e")
  (if multiple-cursors-mode
      (message "Cannot toggle fake cursor while `multiple-cursors-mode' is active.")
    (mouse-minibuffer-check event)
    "The following are taken from the old mc/toggle-cursor-on-click without
calling the last activation"
    (let ((position (event-end event)))
      (unless (windowp (posn-window position))
        (error "Position not in text area of window"))
      (select-window (posn-window position))
      (when-let ((pt (posn-point position)))
        (when (numberp pt)
          (let ((existing (mc/fake-cursor-at-point pt)))
            (if existing
                (progn
                  (mc/remove-fake-cursor existing)
                  (message "Removed fake cursor. Run M-x multiple-cursors-mode to edit."))
              (save-excursion
                (goto-char pt)
                (mc/create-fake-cursor-at-point))
              (message "Added fake cursor. Run M-x multiple-cursors-mode to edit."))))))))

(defun tt/mc-toggle-cursor-at-point ()
  "Add or remove a multiple-cursors fake cursor at point."
  (interactive)
  (if multiple-cursors-mode
      (message "Cannot toggle fake cursor while `multiple-cursors-mode' is active.")
    (let ((existing (mc/fake-cursor-at-point)))
      (if existing
          (progn
            (mc/remove-fake-cursor existing)
            (message "Removed fake cursor. Run M-x multiple-cursors-mode to edit."))
        (mc/create-fake-cursor-at-point)
        (message "Added fake cursor. Run M-x multiple-cursors-mode to edit.")))))

(defun tt/mc-use-one-fake-cursor-as-real-cursor (&rest _)
    "Use one fake cursor as point before enabling `multiple-cursors-mode'.

This keeps the number of editing locations equal to the number of
manually placed fake cursors, instead of adding the original point as an
extra editing location."
    (unless multiple-cursors-mode
      (when-let ((cursor (car (sort (mc/all-fake-cursors)
                                    (lambda (a b)
                                      (< (overlay-start a)
                                         (overlay-start b)))))))
        (mc/pop-state-from-overlay cursor))))

(use-package multiple-cursors
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "multiple-cursors")
  ;; Mouse bindings also need the corresponding down event unbound.
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind (("M-<mouse-1>" . tt/mc-toggle-cursor-on-click)
         ("C-S-SPC" . tt/mc-toggle-cursor-at-point))
  :config

  (advice-add #'multiple-cursors-mode :before #'tt/mc-use-one-fake-cursor-as-real-cursor)
  (add-to-list 'mc/cmds-to-run-once #'tt/mc-toggle-cursor-on-click)
  (add-to-list 'mc/cmds-to-run-once #'tt/mc-toggle-cursor-at-point)
  (add-to-list 'mc/cmds-to-run-once #'multiple-cursors-mode))


;;; 5. Theme setup

;; The tt/ namespace functions will be moved here
(defun tt/moe-light-base ()
  "Load Moe light theme with TT's common Moe options."
  (setq moe-theme-highlight-buffer-id t
        moe-theme-modeline-color 'purple)
  (moe-light))

(defun tt/moe-pink-terminal ()
  "Apply higher-contrast pink Moe overrides for terminal frames."
  (interactive)
  (tt/moe-light-base)
  (set-face-attribute 'default nil :background "#f6e0e0" :foreground "#3a2d33")
  ;;(set-face-attribute 'cursor nil :background "#b03060")
  ;;(set-face-attribute 'region nil :background "#e3aeb8")
  ;;(set-face-attribute 'hl-line nil :background "#c3f2b0")
  (set-face-attribute 'fringe nil :background "#efc5c6")
  (set-face-attribute 'vertical-border nil :foreground "#c78d95")
  (set-face-attribute 'line-number nil :foreground "#6b5860" :background "#cfb6b6")
  ;;(set-face-attribute 'line-number-current-line nil :foreground "#4f3140" :background "#e3aeb8")
  )

(defun tt/moe-pink-gui ()
  "Apply light pink Moe overrides for GUI/rich-color frames."
  (interactive)
  (tt/moe-light-base)
  (set-face-attribute 'default nil :background "#fff5f5" :foreground "#3a2d33")
  ;;(set-face-attribute 'cursor nil :background "#b03060")
  ;;(set-face-attribute 'region nil :background "#efc2cf")
  ;;(set-face-attribute 'hl-line nil :background "#d7f8c8")
  (set-face-attribute 'fringe nil :background "#f4dada")
  (set-face-attribute 'vertical-border nil :foreground "#d2aeb6")
  (set-face-attribute 'line-number nil :foreground "#7a6269" :background "#f4dada")
  (set-face-attribute 'line-number-current-line nil :foreground "#4f3140" :background "#e9b8c2"))


(defun tt/moe-pink ()
  "Apply TT's pink Moe face overrides for the current frame type."
  (interactive)
  (if (display-graphic-p)
      (tt/moe-pink-gui)
    (tt/moe-pink-terminal)))




(use-package moe-theme
  :config
  (tt/moe-pink))

;; Replace hex color with name  
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (rainbow-mode 1))



;; Add only one proven theme setup here later.

;;; 5. Tree-sitter setup

;; The package treesit-auto will provide support for auto installation
;; of grammars,and load by auto-fallback mechanism
;; package repo: https://github.com/renzmann/treesit-auto
;; for our usage, M-x `treesit-auto-install-all' may be just enough
;; 2026-06-26: a bug fix was added to make sure that the ABI 14 compatible
;; treesit packages can be added.


(defconst tt/treesit-supported-p
  (and (fboundp 'treesit-available-p)
       (treesit-available-p))
  "Non-nil when this Emacs can use tree-sitter.")

(defconst tt/treesit-abi-version
  (when (and tt/treesit-supported-p
             (fboundp 'treesit-library-abi-version))
    (treesit-library-abi-version))
  "Tree-sitter grammar ABI version supported by this Emacs.")

(defconst tt/treesit-auto-langs
  '(python c cpp bash dockerfile make cmake lua javascript typescript tsx
           json yaml markdown org css html toml julia)
  "Tree-sitter grammars managed by `treesit-auto' in this config.")

(defconst tt/treesit-abi14-revisions
  '((python . "v0.23.6")
    (c . "v0.23.6")
    (cpp . "v0.23.4")
    (bash . "v0.23.3")
    (dockerfile . "v0.2.0")
    (make . "v1.1.1")
    (cmake . "v0.7.2")
    (lua . "v0.3.0")
    (javascript . "v0.23.1")
    (typescript . "v0.23.2")
    (tsx . "v0.23.2")
    (json . "v0.24.8")
    (yaml . "v0.7.2")
    (markdown . "v0.4.1")
    (org . "v1.3.1")
    (css . "v0.23.2")
    (html . "v0.23.2")
    (toml . "v0.7.0")
    (julia . "v0.23.1"))
  "Known grammar revisions whose generated parsers work with ABI 14.")

(defconst tt/treesit-abi15-revisions
  '((cpp . nil))
  "Grammar revision overrides for ABI 15 and newer.")


(use-package treesit-auto
  :if tt/treesit-supported-p
  :ensure nil
  :init
  (tt/ensure-vendor-and-load "treesit-auto")
  :config
  (setq treesit-auto-install nil
        treesit-auto-langs tt/treesit-auto-langs)
  (tt/treesit-auto-apply-abi-revisions)
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist)
  (let* ((missing (seq-filter (lambda (lang) (not (treesit-ready-p lang t)))
                              treesit-auto-langs))
         (count (length missing)))
    (when (> count 0)
      (message "There are %d tree-sitter grammars (%s) not installed or not usable. Manually install with M-x treesit-auto-install-all or M-x treesit-install-language-grammar."
               count
               (mapconcat #'symbol-name missing ", ")))))


;;; 6. Python IDE setup

;; Aim for a modern but understandable Python editing stack:
;; treesit-auto chooses tree-sitter modes, Eglot provides LSP, Flymake
;; displays diagnostics, Ty provides type-aware language service, and
;; Ruff provides fast formatting/linting.  Python environments are treated
;; by interpreter path only, so conda, virtualenv, and uv-created envs are
;; equal as long as they contain a Python executable. Tool installation is
;; explicit; nothing is installed automatically at startup.

(defvar tt/python-ide-format-on-save nil
  "Non-nil means format Python buffers with Ruff before saving.")

(defun tt/python-ide-ruff-format-buffer ()
  "Format the current buffer with central `ruff format'."
  (interactive)
  (let ((ruff (tt/conda-env-executable "ruff")))
    (unless ruff
      (user-error "ruff executable not found; run M-x tt/python-ide-install-tools"))
    (let* ((file (or buffer-file-name "stdin.py"))
           (input (current-buffer))
           (output (generate-new-buffer " *ruff format*")))
      (unwind-protect
          (let ((status (with-current-buffer input
                          (call-process-region
                           (point-min) (point-max)
                           ruff nil (list output t) nil
                           "format" "--stdin-filename" file "-"))))
            (if (zerop status)
                (let ((point (point)))
                  (erase-buffer)
                  (insert-buffer-substring output)
                  (goto-char (min point (point-max)))
                  (message "Formatted buffer with Ruff."))
              (with-current-buffer output
                (user-error "ruff format failed: %s" (string-trim (buffer-string))))))
        (kill-buffer output)))))

(defun tt/python-ide-format-buffer ()
  "Format the current Python buffer using Ruff."
  (interactive)
  (tt/python-ide-ruff-format-buffer))

(defun tt/python-ide-before-save ()
  "Maybe format Python buffers before saving."
  (when tt/python-ide-format-on-save
    (tt/python-ide-format-buffer)))

(defun tt/python-ide-refresh-eglot-server-program ()
  "Configure Eglot to launch the central Ty executable for Python."
  (require 'eglot)
  (let ((ty (tt/conda-env-executable "ty")))
    (setq eglot-server-programs
          (cl-remove '(python-mode python-ts-mode) eglot-server-programs
                     :key #'car :test #'equal))
    (add-to-list 'eglot-server-programs
                 `((python-mode python-ts-mode) . (,(or ty "ty") "server")))))

(defun tt/python-ide-start-eglot ()
  "Start Eglot for Python, using central Ty when available.

The Python interpreter is discovered separately and made buffer-local via
`python-shell-interpreter'.  Ty may also use project config/environment
according to its own rules."
  (interactive)
  (if (tt/conda-env-executable "ty")
      (progn
        (tt/python-ide-refresh-eglot-server-program)
        (eglot-ensure))
    (message "ty executable not found. Run M-x tt/python-ide-install-tools.")))

(defun tt/python-ide-setup-buffer ()
  "Buffer-local setup for Python IDE behavior."
  (setq-local tab-width 4)
  (when-let ((python (tt/python-ide-current-interpreter)))
    (setq-local python-shell-interpreter python))
  (add-hook 'before-save-hook #'tt/python-ide-before-save nil t)
  ;; Candidate bindings to test later; keep the default completion key
  ;; (`M-TAB' / `completion-at-point') untouched for now.
  ;; (local-set-key (kbd "C-c p e") #'tt/python-ide-start-eglot)
  ;; (local-set-key (kbd "C-c p f") #'tt/python-ide-format-buffer)
  ;; (local-set-key (kbd "C-c p d") #'flymake-show-buffer-diagnostics)
  ;; (local-set-key (kbd "C-c p n") #'flymake-goto-next-error)
  ;; (local-set-key (kbd "C-c p p") #'flymake-goto-prev-error)
  ;; (local-set-key (kbd "C-c p a") #'eglot-code-actions)
  ;; (local-set-key (kbd "C-c p i") #'tt/python-ide-install-tools)
  ;; (local-set-key (kbd "C-c p r") #'tt/python-ide-report)
  (tt/python-ide-start-eglot))


(use-package flymake
  :ensure nil
  :commands (flymake-show-buffer-diagnostics
             flymake-goto-next-error
             flymake-goto-prev-error)
  :config
  (setq flymake-no-changes-timeout 0.5
        flymake-start-on-save-buffer t
        flymake-start-on-flymake-mode t))

(use-package python
  :ensure nil
  :config
  (add-hook 'python-mode-hook #'tt/python-ide-setup-buffer)
  (when (fboundp 'python-ts-mode)
    (add-hook 'python-ts-mode-hook #'tt/python-ide-setup-buffer)))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :config
  (tt/python-ide-refresh-eglot-server-program)
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil))

(let ((missing (tt/python-ide-missing-tools)))
  (when missing
    (message "Python IDE tools missing: %s. Run M-x tt/python-ide-install-tools."
             (mapconcat #'identity missing ", "))))


;;; 7. Others

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here


;; A test package, to be removed later
(use-package avy
  :vc (avy :url "https://github.com/abo-abo/avy.git"
	   :rev :newest)
  :commands (avy-goto-char avy-goto-word-1))
