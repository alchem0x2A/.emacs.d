;; Setting up Ivy-mode, Counsel and Swiper
;; Globally enable ivy mode
(ivy-mode t)

;; Use history in ivy completion
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; Set higher ivy popup list
(setq ivy-height 15)


(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; Bind "C-h <key>" to counsel stuff
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h l") 'counsel-find-library) ;replace the original view-lossage function
(global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-h u") 'counsel-unicode-char)

;; Define actions in the minibuffer keymap
;; Mimicking the behavior of "C-c i" in helm-mode, but with mode flexibility
(define-key ivy-minibuffer-map (kbd "C-c i") 'ivy-dispatching-done)
(ivy-add-actions t
                   '(("I" (lambda (x) (with-ivy-window (insert x))) "insert full name")))
(ivy-add-actions 'counsel-find-file
		 '(("F" (lambda (x) (with-ivy-window (insert (file-relative-name x))))
		    "insert relative file name")
		   ("B" (lambda (x)
			  (with-ivy-window
			    (insert (file-name-nondirectory (replace-regexp-in-string "/\\'" "" x)))))
		    "insert file name without any directory information")))
