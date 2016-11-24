;;Workaround for the ipython-5 issue
(defun create-ipython-ansi-term ()
  "Create an ansi-term buffer named *Python* to work around characters in IPython5"
  (interactive)
  (setq cur (buffer-name))
  (setq b (get-buffer "*Python*"))
  (if (not (eq nil b))
      (kill-buffer b)
    )
  (setq b (ansi-term "ipython"))
  (get-buffer b)
  (rename-buffer "*Python*")
  (switch-to-buffer (get-buffer cur))
  )

;;An ipython daemon is idle on start-up
;;Use python virtualenv if possible
(when (file-exists-p "~/.virtualenvs/ve-matplotlib2")
  (pyvenv-workon "ve-matplotlib2")
  )
(create-ipython-ansi-term)


;;Use elpy as the main support for python mode
(elpy-enable)

;;Use IPython for the compiling
;;Of course python 3 should be used
(elpy-use-ipython)

;;If the ansi-term is not invoked for ipython process, use simple-prompt as instead
(setq python-shell-interpreter-interactive-arg "--simple-prompt -i")

;;Use flycheck instead of flymake for pep8 check
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
  )

;;Enable auto pep8 on save
(require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
