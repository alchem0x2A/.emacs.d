;;Editor setting and global keybindings for os x like other editors

;; Enable visual-line-mode for default
(visual-fill-column-mode t)
(setq line-move-visual nil)
(setq visual-fill-column-width 80)

;; Always use electric pair
(electric-pair-mode t)

;; Allow undo-tree to use
(require 'undo-tree)
(global-undo-tree-mode t)

(move-text-default-bindings)

;; Delete functions
(defun backward-delete-line (arg)
  "Delete ARG lines backward."
  (interactive "p")
  (delete-region (point) (line-beginning-position))
  )

(defun forward-delete-line (arg)
  "Delete the rest part of the line"
  (interactive "p")
  (delete-region (point) (line-end-position))
  )

(defun forward-delete-word-or-subword (&optional arg)
  (interactive "p")
  (if (syntax-subword-mode)
      (delete-region (point) (save-excursion
			       (syntax-subword-forward arg)
			       (point)))
    (delete-region (point) (save-excursion
			     (subword-forward arg)
			     (point)))
    )
  )

(defun backward-delete-word-or-subword (&optional arg)
  (interactive "p")
  (forward-delete-word-or-subword (- arg))
  )


;;Some fancy select/copy/paste stuffs from atom
;;Select the whole line, wherever the cursor is
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

;; Enable fast file mark in split dired windows
(setq dired-dwim-target t)
