;; This file defines the GLOBAL key-bindings for Emacs
;; Other mode-specified key-bindings are usually set in individual files
;; Currently the macOS keys are implemented

(when (eq system-type 'darwin)
  ;; s-z is normal undo-tree-undo
  ;; s-Z is undo-tree-redo
  ;; I don't link to bind the C-x U with visialization
;; of undo tree. Disable it.
  (define-key undo-tree-map (kbd "C-x u") nil)
  (define-key undo-tree-map (kbd "s-Z") 'undo-tree-redo)
  ;; Undo

  ;; Some common macOS kbds
  ;; Delete window like C-x 0 but not killing buffer.
  (global-set-key (kbd "s-w") 'delete-window)
  ;;Overwrite the default s-S which open the system panel
  (global-set-key (kbd "s-S") 'write-file)
  ;;Oevrwrite the default s-o with emacs-like open
  (global-set-key (kbd "s-o") 'helm-find-files)
  ;; Use global compile command
  (global-set-key (kbd "s-b") 'compile)

  ;;Use Cmd+arrow to move to beginning and end of a line
  (global-set-key (kbd "s-<left>") 'move-beginning-of-line)
  (global-set-key (kbd "s-<right>") 'move-end-of-line)
  (global-set-key (kbd "s-<up>") 'beginning-of-buffer)
  (global-set-key (kbd "s-<down>") 'end-of-buffer)


  ;;Copy and paste facilities
  (global-set-key [remap ns-copy-including-secondary] 'kill-ring-save)

  ;;Delete functions
  (global-set-key (kbd "s-<backspace>") 'backward-delete-line)
  (global-set-key (kbd "s-<kp-delete>") 'forward-delete-line)
  ;; Delete a word without thinksaving
  (global-set-key (kbd "M-<backspace>") 'backward-delete-word-or-subword)
  (global-set-key (kbd "C-<backspace>") 'backward-delete-word-or-subword)
  (global-set-key (kbd "M-<kp-delete>") 'forward-delete-word-or-subword)
  (global-set-key (kbd "C-<kp-delete>") 'forward-delete-word-or-subword)

  ;;Move windows via arrow keys
  (global-set-key (kbd "M-s-<up>") 'windmove-up)
  (global-set-key (kbd "M-s-<down>") 'windmove-down)
  (global-set-key (kbd "M-s-<left>") 'windmove-left)
  (global-set-key (kbd "M-s-<right>") 'windmove-right)

  ;; Line selection
  (global-set-key (kbd "s-l") 'select-current-line)

  ;; Comment whole line
  ;; Bind the comment and uncomment of current line / selected region s-/
;; Only works for emacs 25.1 +
;; Normal in-line comment is still binded to M-;
  (when (version<= "25.0" emacs-version)
    (global-set-key (kbd "s-/") 'comment-line))

  ;; Extras
  ;;Do not use the key for opening a font panel
  (global-unset-key (kbd "s-t"))
  ;;Disable suspend-frame key, which normally only caused problem in OS X
  (global-unset-key (kbd "C-z"))
 
)
