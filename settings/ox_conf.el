;; This configuration mostly deals with typesettting with org export
(require 'ox)
(require 'ox-publish)
(require 'ox-pandoc)
(require 'org-ref)			;In case the ref mode is not loaded.
(require 'ox-extra)			;Requires the use of org elpa

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
      (list "latexmk -f -pdf -quiet -view=none -pdflatex='xelatex -interaction=nonstopmode --shell-escape' %f"))
(setq org-latex-with-hyperref nil)


;; Use user defined labels instead
(setq org-latex-prefer-user-labels t)

;; Use minted and pigment
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

;; Enable the exclusion of certain headlines
(ox-extras-activate '(ignore-headlines))

;; Own-defined inlinetask latex export

(defun my/org-latex-format-inlinetask-function
  (todo _todo-type priority title tags contents info)
  "Default format function for a inlinetasks.
See `org-latex-format-inlinetask-function' for details."
  (let ((full-title
	 (concat (when todo (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
		 (when priority (format "\\framebox{\\#%c} " priority))
		 title
		 (when tags
		   (format "\\hfill{}\\textsc{:%s:}"
			   (mapconcat
			    (lambda (tag) (org-latex-plain-text tag info))
			    tags ":"))))))
    (concat "\\begin{center}\n"
	    "\\fbox{\n"
	    "\\begin{minipage}[c]{.8\\linewidth}\n"
	    full-title "\n\n"
	    (and (org-string-nw-p contents)
		 (concat "\\rule[.8em]{\\textwidth}{2pt}\n\n" contents))
	    "\\end{minipage}\n"
	    "}\n"
	    "\\end{center}")))

(setq org-latex-format-inlinetask-function 'my/org-latex-format-inlinetask-function)
