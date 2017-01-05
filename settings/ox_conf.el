;; This configuration mostly deals with typesettting with org export
(require 'ox)
(require 'ox-publish)
(require 'ox-pandoc)
(require 'org-ref)			;In case the ref mode is not loaded.

;; Comes from https://github.com/jkitchin/jmax/blob/master/ox-manuscript.el
;; * Journal templates
;; Bare-bones template
(add-to-list 'org-latex-classes
	     '("no-article"
	       "\\documentclass{article}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** <<ACS journals>>
(add-to-list 'org-latex-classes
	     '("achemso"
	       "\\documentclass{achemso}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** <<APS journals>>
(add-to-list 'org-latex-classes '("revtex4-1"
				  "\\documentclass{revtex4-1}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				   ("\\section{%s}" . "\\section*{%s}")
				   ("\\subsection{%s}" . "\\subsection*{%s}")
				   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				   ("\\paragraph{%s}" . "\\paragraph*{%s}")
				   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** <<Springer journals>>
(add-to-list 'org-latex-classes '("svjour3"
				  "\\documentclass{svjour3}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** <<Elsevier journals>>
(add-to-list 'org-latex-classes '("elsarticle"
"\\documentclass{elsarticle}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** Nature
;; http://www.nature.com/srep/authors/submissions.html
;; the example shows unnumbered sections which might require a full exporter to get.
(add-to-list 'org-latex-classes '("nature"
"\\documentclass[fleqn,10pt]{wlscirep}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; Latexmk and xelatex are fancier
(setq org-latex-pdf-process
      (list "latexmk -f -pdf -quiet -view=none -xelatex %f"))
(setq org-latex-with-hyperref nil)


;; Use user defined labels instead
(setq org-latex-prefer-user-labels t)
