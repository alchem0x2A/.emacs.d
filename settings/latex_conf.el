;; Configuration for LATEX files
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
;; Will ask fo master file each time?
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-ignore-tex)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'cdlatex-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode) ;Allow correlation mode
(setq reftex-plug-into-AUCTeX t)
(setq TeX-source-correlate-start-server t) ;Always start server at startup

;; Open pgf and tikz files in TeX mode
(add-to-list 'auto-mode-alist '("\\.pgf\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.tikz\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.pdf_tex\\'" . LaTeX-mode)) ;Inkscape generated file

;; Enable choosing XeLaTeX and LuaTeX as TeX engining as well as Latexmk
;; By default, this option is only achievable when opening the document buffer only
;; If you encouter problems, reload the buffer and call "C-c C-c" again
(setq TeX-check-engine t)
(auctex-latexmk-setup)
;; Useful when invoking sources made by org mode
;; By default the
;; -nonstop-mode and -shell-escape are passed onto
(setq-default TeX-command-extra-options "-shell-escape")
(setq TeX-view-program-selection
      '((output-pdf "Skim")
	(output-dvi "open")
	(output-html "open")))
(setq TeX-view-program-list
     '(("Skim" "displayline -b -g %n %o %b")))
