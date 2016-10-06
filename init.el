;;The main part of the emacs starting script
;;All other parts are separated in other individual files

(require 'package)
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

(add-to-list 'load-path setting-dir)
(add-to-list 'load-path vendor-dir)

(exec-path-from-shell-initialize)

(load "helm_conf.el")
(load "projectile_conf.el")
(load "ui.el")
(load "magit_conf.el")
(load "editor.el")
