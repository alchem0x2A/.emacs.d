;;; init.el --- T. Tian's Emacs init -*- lexical-binding: t; -*-

;;; TT's Emacs Initialization File:
;; Single-file Emacs configuration, rebuilt incrementally.
;; Section headers use three semicolons so `outline-minor-mode' can fold them.
;;; Use `tt/toggle-outline' to collapse the headings!

;;; 1. Universal pre-start setup

;; This setup requires Emacs > 29.1 as the `treesitter' support
;; is needed

;;;; 1.1 Setup for package system & melpa

(when (version< emacs-version "29.1")
  (error "This init requires Emacs 29.1 or newer; current version is %s"
         emacs-version))

 
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; The `package' package itself should be installed

(require 'package)
(require 'use-package)
(require 'package-vc)

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

(package-initialize)

;;; 2 System-wide variables and functions (`tt/' namespace)

(defconst tt/macos-p (eq system-type 'darwin)
  "Non-nil when running on macOS / Darwin.")

(defconst tt/linux-p (eq system-type 'gnu/linux)
  "Non-nil when running on GNU/Linux.")

(defconst tt/ns-p (eq window-system 'ns)
  "Non-nil when running the macOS NS GUI port.")

(Defconst tt/macos-command-is-super-p
  (and tt/ns-p
       (boundp 'ns-command-modifier)
       (eq ns-command-modifier 'super))
  "Non-nil when macOS Command currently generates Emacs Super events.")

(defun tt/select-current-line ()
  "Select the current line from beginning to end."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun tt/bind-keys (bindings)
  "Apply BINDINGS to `global-map'.
BINDINGS is an alist of (KEY . COMMAND).  A nil COMMAND unbinds KEY."
  (dolist (binding bindings)
    (define-key global-map (kbd (car binding)) (cdr binding))))


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

;;; 2. Non-platform keybindings

(tt/bind-keys
 '(("C-c t l" . tt/select-current-line)
   ("C-c t =" . global-text-scale-adjust)
   ("C-c t +" . global-text-scale-adjust)
   ("C-c t -" . global-text-scale-adjust)
   ("C-c t 0" . global-text-scale-adjust)))

;;; 3. macOS keybindings

(when tt/macos-command-is-super-p
  ;; The NS port maps Command through `ns-command-modifier'.  When it is
  ;; `super', Command bindings are ordinary Emacs `s-' bindings.
  (tt/bind-keys
   '(("s-l" . tt/select-current-line)
     ("s-=" . global-text-scale-adjust)
     ("s-+" . global-text-scale-adjust)
     ("s--" . global-text-scale-adjust)
     ("s-0" . global-text-scale-adjust)
     ;; Reserve these for future deliberate choices.
     ("s-p" . nil)
     ("s-q" . nil)
     ("s-o" . nil)
     ("s-y" . nil))))

;;; 4. GNU/Linux keybindings

(when tt/linux-p
  ;; Add Linux-specific GUI or terminal bindings here when they are proven useful.
  nil)

;;; 5. Global display setup

(setq inhibit-startup-screen t
      visible-bell t
      ring-bell-function #'ignore)

(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(global-display-line-numbers-mode t)
(show-paren-mode t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(context-menu-mode t)
(global-auto-revert-mode t)
(winner-mode t)

;;; 6. Theme setup

;; Add only one proven theme setup here later.

;;; 7. Tree-sitter setup

;; Add tree-sitter language setup here later.

;;; 8. Others

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
