;;Editor setting and global keybindings for os x like other editors

;;Overwrite original "s-w" behavior from close the frame to close the buffer and window (more intuitive to me)
(global-set-key (kbd "s-w") 'kill-buffer-and-window)
;;Overwrite the default s-S which open the system panel
(global-set-key (kbd "s-S") 'write-file)
;;Oevrwrite the default s-o with emacs-like open
(global-set-key (kbd "s-o") 'helm-find-files)
;; Use global compile command
(global-set-key (kbd "s-b") 'compile)
;;Invoke redo function as normal text editors
;; (defun redo-undo ()
;;   (interactive)
;;   (keyboard-quit)
;;   (undo))
;; (global-set-key (kbd "s-Z") 'redo-undo)

;;Copy and paste facilities
(global-set-key [remap ns-copy-including-secondary] 'kill-ring-save)


;;Use Cmd+arrow to move to beginning and end of a line
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

;;Meta+arrow for navigation
(global-set-key (kbd "M-<up>") 'scroll-down-command)
(global-set-key (kbd "M-<down>") 'scroll-up-command)

;;Normal OS X like kill behavior with Cmd + delete
;; TODO: add the killed region to the second place of the kill ring?
(defun dont-save-kill-ring ()
  (setq kill-ring (cdr kill-ring)
	))

(defun backward-delete-line (arg)
  "Delete ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg))
  (dont-save-kill-ring)
  )

(defun forward-delete-line (arg)
  "Delete the rest part of the line"
  (interactive "p")
  (kill-line arg)
  (dont-save-kill-ring)
  )

(defun backward-delete-word (arg)
  "Delete the word backwords"
  (interactive "p")
  (backward-kill-word arg)
  (dont-save-kill-ring)
  )

;; ;; (defun forward-delete-word (arg)
;;   "Delete the word forward"
;;   (for)
;;   )



(global-set-key (kbd "s-<backspace>") 'backward-delete-line)
(global-set-key (kbd "s-<kp-delete>") 'forward-delete-line)

(global-set-key [remap backward-kill-word] 'backward-delete-word)

;;Move windows via arrow keys
(global-set-key (kbd "M-s-<up>") 'windmove-up)
(global-set-key (kbd "M-s-<down>") 'windmove-down)
(global-set-key (kbd "M-s-<left>") 'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)

;;Do not use the key for opening a font panel
(global-unset-key (kbd "s-t"))
;;Disable suspend-frame key, which normally only caused problem in OS X
(global-unset-key (kbd "C-z"))


;;Some fancy select/copy/paste stuffs from atom
;;Select the whole line, wherever the cursor is
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(global-set-key (kbd "s-l") 'select-current-line)


;; Bind the comment and uncomment of current line / selected region s-/
;; Only works for emacs 25.1 +
;; Normal in-line comment is still binded to M-;

(when (not (version< emacs-version "25.0"))
  (global-set-key (kbd "s-/") 'comment-line))


;; Enable fast file mark in split dired windows
(setq dired-dwim-target t)
