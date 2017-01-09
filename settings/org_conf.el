;; Use inline-tasks for TODO entries
;; C-c C-x t will work
(require 'org-inlinetask)
(require 'ox)

;; Use org mode html output for emails


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
(add-hook 'org-mode-hook 'turn-on-cdlatex)
(add-hook 'org-mode-hook 'visual-line-mode)


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
   (latex . t)
   (shell . t))
   )

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
(setq org-startup-indented t)

;; Disable automatic export
(setq org-export-babel-evaluate t)

;; Do not use flycheck on org source code block for python
;; (defun org-disable-flycheck-source-block
;;     (setq-local flycheck-disabled-checkers '(python-flake8)))

;; (add-hook 'org-src-mode-hook 'org-disable-flycheck-source-block)

;; Enable inline pdf display by redifining the org-display-inline-images method
;; [[http://stackoverflow.com/questions/15407485/inline-pdf-images-in-org-mode]]
(setq org-imagemagick-display-command "convert -density 600 \"%s\" -thumbnail \"%sx%s>\" \"%s\"")
(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This
can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (unless refresh
    (org-remove-inline-images)
    (if (fboundp 'clear-image-cache) (clear-image-cache)))
  (save-excursion
    (save-restriction
      (widen)
      (setq beg (or beg (point-min)) end (or end (point-max)))
      (goto-char beg)
      (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
                        (substring (org-image-file-name-regexp) 0 -2)
                        "\\)\\]" (if include-linked "" "\\]")))
            old file ov img)
        (while (re-search-forward re end t)
          (setq old (get-char-property-and-overlay (match-beginning 1)
                                                   'org-image-overlay)
        file (expand-file-name
                      (concat (or (match-string 3) "") (match-string 4))))
          (when (file-exists-p file)
            (let ((file-thumb (format "%s%s_thumb.png" (file-name-directory file) (file-name-base file))))
              (if (file-exists-p file-thumb)
                  (let ((thumb-time (nth 5 (file-attributes file-thumb 'string)))
                        (file-time (nth 5 (file-attributes file 'string))))
                    (if (time-less-p thumb-time file-time)
            (shell-command (format org-imagemagick-display-command
                           file org-image-actual-width org-image-actual-width file-thumb) nil nil)))
                (shell-command (format org-imagemagick-display-command
                                         file org-image-actual-width org-image-actual-width file-thumb) nil nil))
              (if (and (car-safe old) refresh)
                  (image-refresh (overlay-get (cdr old) 'display))
                (setq img (save-match-data (create-image file-thumb)))
                (when img
                  (setq ov (make-overlay (match-beginning 0) (match-end 0)))
                  (overlay-put ov 'display img)
                  (overlay-put ov 'face 'default)
                  (overlay-put ov 'org-image-overlay t)
                  (overlay-put ov 'modification-hooks
                               (list 'org-display-inline-remove-overlay))
                  (push ov org-inline-image-overlays))))))))))

;; Change default python header in org source block
(setq org-babel-default-header-args:python
      (cons '(:results . "output org drawer replace")
            (assq-delete-all :results org-babel-default-header-args)))

;; Auto fold 
(add-hook 'org-mode-hook  'org-hide-block-all)

;; Define colored outpur for TODO keywords

(defun org-latex-format-headline-colored-keywords-function
    (todo todo-type priority text tags info)
        (concat
	 (cond ((string= todo "ON-GOING")(and todo (format "{\\color{orange}\\bfseries\\sffamily %s} " todo)))
	  ((string= todo "TODO")(and todo (format "{\\color{red}\\bfseries\\sffamily %s} " todo)))
   ((string= todo "DONE")(and todo (format "{\\color{green}\\bfseries\\sffamily %s} " todo))))
            (and priority (format "\\framebox{\\#%c} " priority))
            text
            (and tags
            (format "\\hfill{}\\textsc{%s}"
    (mapconcat (lambda (tag) (org-latex-plain-text tag info))
           tags ":")))))

(setq org-latex-format-headline-function 'org-latex-format-headline-colored-keywords-function)

;; More templates:
(add-to-list 'org-structure-template-alist '("he" "#+HEADERS: "))
(add-to-list 'org-structure-template-alist '("n" "#+NAME: "))
(add-to-list 'org-structure-template-alist '("tb" "#+TBLNAME: "))
(add-to-list 'org-structure-template-alist '("ca" "#+CAPTION: "))
(add-to-list 'org-structure-template-alist '("al" "#+ATTR_LATEX: "))
