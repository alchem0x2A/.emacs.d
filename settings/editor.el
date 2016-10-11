;;Editor setting and global keybindings for os x like other editors

;;Overwrite original "s-w" behavior from close the frame to close the buffer and window (more intuitive to me)
(global-set-key (kbd "s-w") 'kill-buffer-and-window)
;;Overwrite the default s-S which open the system panel
(global-set-key (kbd "s-S") 'write-file)
;;Oevrwrite the default s-o with emacs-like open
(global-set-key (kbd "s-o") 'helm-find-files)
;;Invoke redo function as normal text editors
;; (defun redo-undo ()
;;   (interactive)
;;   (keyboard-quit)
;;   (undo))
;; (global-set-key (kbd "s-Z") 'redo-undo)


;;Use Cmd+arrow to move to beginning and end of a line
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

;;Meta+arrow for navigation
(global-set-key (kbd "M-<up>") 'scroll-down-command)
(global-set-key (kbd "M-<down>") 'scroll-up-command)

;;Normal OS X like kill behavior with Cmd + delete
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "s-<backspace>") 'backward-kill-line)
(global-set-key (kbd "s-<kp-delete>") 'kill-line)

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
