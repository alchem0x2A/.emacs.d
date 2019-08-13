(require 'helm)
(require 'helm-config)
(require 'helm-flx)


;;Key bindings
(setq helm-ff-lynx-style-map nil)
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
(define-key helm-map (kbd "<left>") 'backward-char)
(define-key helm-map (kbd "<right>") 'forward-char)
