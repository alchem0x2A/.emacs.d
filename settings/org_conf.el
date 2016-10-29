;; Use inline-tasks for TODO entries
;; C-c C-x t will work
(require 'org-inlinetask)

;; Global key settings for the org mode

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Enable flycheck at beginning
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)
(add-hook 'org-mode-hook (lambda ()
			   (setq ispell-parser 'tex)))
(add-hook 'org-mode-hook 'flyspell-ignore-tex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)


;; Useful function to add a code block
;; By-default org will try to open another frame and edit
;; this is really a bad behavior.
(setq org-src-window-setup 'other-window)
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python"  "latex" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)))

;; (with-eval-after-load
    ;; (define-key org-mode-map (kbd "C-c C-v m") 'org-insert-src-block)
  ;; (define-key org-mode-map (kbd "C-c C-v C-m") 'org-insert-src-block)
  ;; )

;; Enable bable code evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   ))

;; Set TODO keywords
(setq org-todo-keywords
      '((sequence "TODO"
		  "ON-GOING"
		  "DONE"
		  )))

;; Fixed image width
;; Only works when ImageMagic is supported
(setq org-image-actual-width '(400))

;; Enable support for inline eps files
(add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick)  )
(add-to-list 'image-type-file-name-regexps '("\\.eps\\'" . imagemagick)  )
(add-to-list 'image-file-name-extensions "pdf")
(add-to-list 'image-file-name-extensions "eps")
(setq imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))

;; Highlight the LaTeX and source code
(setq org-src-fontify-natively t)


;; Default Latex Output
(setq org-export-with-smart-quotes t)

;; Smart insert label in LaTeX using helm
(define-key org-mode-map (kbd "C-c C-r") nil)
(global-unset-key (kbd "C-c C-r"))
(define-key org-mode-map (kbd "C-c C-r l") 'org-ref-insert-ref-link)
(define-key org-mode-map (kbd "C-c C-r C-l") 'org-ref-insert-ref-link)
(define-key org-mode-map (kbd "C-c r l") 'org-ref-insert-ref-link)
(define-key org-mode-map (kbd "C-c r C-l") 'org-ref-insert-ref-link)

;; Startup functions
(setq org-startup-with-latex-preview t)
(setq org-startup-with-inline-images t)
