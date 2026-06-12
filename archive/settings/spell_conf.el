;; Set aspell for default spell checking

(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"
			  "--lang=en-US"
			  ))
(defun flyspell-ignore-tex ()
  (interactive)
  (set (make-variable-buffer-local 'ispell-parser) 'tex))
