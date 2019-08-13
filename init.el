;;The main part of the emacs starting script
;;All other parts are separated in other individual files

(require 'package)

;;Test if adding further changes possible

;;Only need to use for aquamacs
;; (setq user-emacs-directory "~/.emacs.d")
;; (setq package-user-dir "~/emacs.d/elpa")

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(when (version< emacs-version "24")
  (add-to-list 'package-archives
	       '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; temporary fix for elpa
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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

;; locate custom file 
(setq custom-file
      (expand-file-name "custom_setting.el" init-dir))
(load custom-file :noerror)


;;(package-install-selected-packages)

(load "envs.el")
(load "editor.el")
(load "ivy.el")
(load "keymaps.el")
(load "spell_conf.el")
;; (load "projectile_conf.el")
(load "ui.el")
(load "magit_conf.el")
(load "python_conf.el")
(load "c_cpp_conf.el")
(load "org_conf.el")
(load "ox_conf.el")
(load "latex_conf.el")




