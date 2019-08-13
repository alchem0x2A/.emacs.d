;; Global settings for Elpy mode in emacs


;;Use elpy as the main support for python mode
(elpy-enable)

;; Configure the workon home for elpy
(setenv "WORKON_HOME"
	"~/.venv/")

;; Default virtualenv to work on
(if (member "default-py37"
	    (pyvenv-virtualenv-list))
    (pyvenv-workon "default-py37")
  nil)
;;Use IPython for the compiling
;;Of course python 3 should be used
;; (elpy-use-ipython)

;; Switch to use jupyter console (developed version based on ipython)
;; Following the suggestions here: https://elpy.readthedocs.io/en/latest/ide.html
(setq python-shell-interpreter "jupyter")
(setq python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")
;; Temporary fix for strange echo chars in jupyter console
(setq elpy-shell-echo-output nil)

;;Use flycheck instead of flymake for pep8 check
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
  )

