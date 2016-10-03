(require 'helm)
(require 'helm-config)
(require 'helm-flx)


;;Key bindings

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x h") 'helm-command-prefix-key)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x C-f") 'helm-find-files)


;;Fuzzy search
(setq helm-M-x-fuzzy-match t
      helm-mode-fuzzy-match t
      )

;;Enable Helm by starting
(helm-mode 1)
(helm-flx-mode 1)
