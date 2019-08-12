;; Global counter for undo function
;; Value: 0 -- undo direction
;;        1 -- redo direction 
;; (defvar global-undo-direction 0
  ;; "Global variable for recording undo direction")
;; Redo
;; (defun my-undo()
  ;; (interactive)
  ;; (when (= global-undo-direction 1)
    ;; (setq global-undo-direction 0)
    ;; (keyboard-quit))
  ;; (undo))
;; (defun my-redo()
  ;; Equivalent to C-g + Undo
  ;; If global direction is undo then redo
  ;; (interactive)
  ;; (when (eq global-undo-direction 0)
    ;; (setq global-undo-direction 1)
    ;; (keyboard-quit))
  ;; (undo))

;; Allow undo-tree to use
(require 'undo-tree)
(global-undo-tree-mode t)


;; Mac-based keybindings

(when (eq system-type 'darwin)
  ;;Overwrite original "s-w" behavior from close the frame to close the buffer and window (more intuitive to me)
  (global-set-key (kbd "s-w") 'kill-buffer-and-window)
;;Overwrite the default s-S which open the system panel
  (global-set-key (kbd "s-S") 'write-file)
  ;;Oevrwrite the default s-o with emacs-like open
  (global-set-key (kbd "s-o") 'helm-find-files)
;; Use global compile command
  (global-set-key (kbd "s-b") 'compile)

  ;; s-z is normal undo-tree-undo
  ;; s-Z is undo-tree-redo
  (define-key undo-tree-map (kbd "s-Z")
    'undo-tree-redo)
  ;; Undo
  
)
