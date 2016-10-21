;;The main part of the emacs starting script
;;All other parts are separated in other individual files

(require 'package)

;;Only need to use for aquamacs
;; (setq user-emacs-directory "~/.emacs.d")
;; (setq package-user-dir "~/emacs.d/elpa")

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (version< emacs-version "24")
  (add-to-list 'package-archives
	       '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar init-dir
  (file-name-directory load-file-name)
  "Directory of .emacs.d")

(defvar setting-dir
  (expand-file-name "settings" init-dir)
  "Directory for setting .el files")

(defvar vendor-dir
  (expand-file-name "vendor" init-dir)
  "Directory for hosting non-MELPA packages")

(defvar melpa-dir
  (expand-file-name "elpa" init-dir)
  "Directory for MELPA packages")

(add-to-list 'load-path setting-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path melpa-dir)

;; The custom setting should be loaded before the user scripts.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (2048-game cdlatex company-irony company-irony-c-headers flycheck-irony irony org-ref org sublimity smooth-scrolling smart-mode-line-powerline-theme python-mode pyenv-mode py-autopep8 neotree moe-theme minimap matlab-mode magit latex-unicode-math-mode jedi helm-projectile helm-ls-git helm-gtags helm-flx ggtags flycheck flx-isearch exec-path-from-shell elpy auctex-latexmk 0blayout))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-install-selected-packages)

(load "envs.el")
(load "spell_conf.el")
(load "helm_conf.el")
(load "projectile_conf.el")
(load "ui.el")
(load "magit_conf.el")
(load "editor.el")
(load "python_conf.el")
(load "c_cpp_conf.el")
(load "org_conf.el")





