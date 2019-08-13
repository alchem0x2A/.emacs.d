;; Configuration for LATEX files
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
;; Will ask fo master file each time?
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'cdlatex-mode)
(setq reftex-plug-into-AUCTeX t)

;; Enable choosing XeLaTeX and LuaTeX as TeX engining as well as Latexmk
(setq TeX-check-engine t)
(auctex-latexmk-setup)
;; Useful when invoking sources made by org mode
(setq-default TeX-command-extra-options "-shell-escape")
