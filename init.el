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

;;; 2. Global setup and keybindings

;; Use `emacs' as the one-stop place for built-in setup and keybindings.

(use-package emacs
  :config
  (global-set-key (kbd "C-c t l") #'tt/select-current-line)
  (global-set-key (kbd "C-c t z") #'global-text-scale-adjust)
  (global-set-key (kbd "C-c t u") #'tt/update-packages)
  (global-set-key (kbd "C-c t U") #'tt/update-init)
  (global-set-key (kbd "M-<backspace>") #'tt/backward-delete-word-or-subword)
  (global-set-key (kbd "M-<delete>") #'tt/backward-delete-word-or-subword)
  (when tt/macos-command-is-super-p
    ;; The NS port maps Command through `ns-command-modifier'.  When it is
    ;; `super', Command bindings are ordinary Emacs `s-' bindings.
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
  (setq
   inhibit-startup-screen t
   visible-bell t
   ring-bell-function #'ignore)
  

  ;; Sequential interactive setup commands

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
  (when (fboundp 'visual-fill-column-mode)
    (visual-fill-column-mode t)) ;; fill column
  (setq line-move-visual nil)
  (setq visual-fill-column-width 80)

)

(use-package syntax-subword
  :init
  (add-to-list 'load-path (expand-file-name "syntax-subword" tt/vendor-dir))
  :config
  (global-syntax-subword-mode 1))

(use-package move-text
  :init
  (add-to-list 'load-path (expand-file-name "move-text" tt/vendor-dir))
  :config
  (move-text-default-bindings))


;; Keep frames visually minimal and comfortably sized by default.
(set-face-attribute 'default nil :height 150)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(tool-bar-lines . 0))
(add-to-list 'initial-frame-alist '(vertical-scroll-bars . nil))
(modify-frame-parameters nil '((tool-bar-lines . 0)
                               (vertical-scroll-bars . nil)))

;;; 6. Theme setup

;; Add only one proven theme setup here later.

;;; 7. Tree-sitter setup

;; Add tree-sitter language setup here later.

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
