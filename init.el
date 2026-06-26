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



(require 'package-vc)
(require 'package)

(unless (package-installed-p 'vc-use-package)
  ;; The vc-use-package package itself must be declared BEFORE loading
  ;; use-package to make the :vc keyword available on Emacs 29.x.
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(require 'use-package)
(require 'outline)


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

;;; 3. Global setup and keybindings

;; Use `emacs' as the one-stop place for built-in setup and
;; keybindings.

(use-package emacs
  :config
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

;;;; 3.2 Vendored packages for editing / appearance (light-weight)

(use-package syntax-subword
  :init
  (tt/ensure-vendor-and-load "syntax-subword")
  :config
  (global-syntax-subword-mode 1)
  (global-set-key (kbd "M-<backspace>") #'tt/backward-delete-word-or-subword)
  (global-set-key (kbd "M-<delete>") #'tt/backward-delete-word-or-subword))

(use-package undo-tree
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
  :init
  (tt/ensure-vendor-and-load "move-text")
  :config
  (move-text-default-bindings))

;; Packages for hiding & simplify mode lines
(use-package diminish
  :init
  (tt/ensure-vendor-and-load "diminish"))

;; Packages for hiding & simplify mode lines
(use-package delight
  :init
  (tt/ensure-vendor-and-load "delight"))

;; Similar to fill-column but on a visual face
(use-package visual-fill-column
  :init
  (tt/ensure-vendor-and-load "visual-fill-column")
  :diminish visual-fill-column-mode
  :custom
  (visual-fill-column-width 88)
  :config
  (global-visual-fill-column-mode 1))

;; Toggle visibility of unwanted ephemeral buffers

(use-package popper
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

;;; 6. Theme setup

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

;;; 7. Tree-sitter setup

;; The package treesit-auto will provide support for auto installation
;; of grammars,and load by auto-fallback mechanism
;; package repo: https://github.com/renzmann/treesit-auto
;; for our usage, M-x `treesit-auto-install-all' may be just enough
(use-package treesit-auto
  :config
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (let* ((missing (seq-filter (lambda (lang) (not (treesit-ready-p lang t)))
                              treesit-auto-langs))
         (count (length missing)))
    (when (> count 0)
      (message "There are %d tree-sitter grammars (%s) not installed. Manually install with M-x treesit-auto-install-all or M-x treesit-install-language-grammar."
               count
               (mapconcat #'symbol-name missing ", ")))))


;;; 8. Others

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here


;; A test package, to be removed later
(use-package avy
  :vc (avy :url "https://github.com/abo-abo/avy.git"
	   :rev :newest)
  :commands (avy-goto-char avy-goto-word-1))
