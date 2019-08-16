;;Setting the UI of emacs

;;Disable srgb colorspace when emacs > 24.4.5
;;This is critical to have a working powerline mode
(setq ns-use-srgb-colorspace nil)
(require 'powerline)

;;Use moe-mode
;;enable powerline before moe
(require 'moe-theme)

(setq moe-theme-highlight-buffer-id t)

;;Moe-mode for the pwerline mode
(moe-light)
(moe-theme-set-color 'purple)

;;Don't know why this does not work for eval-buffer
(powerline-moe-theme)

;;Disable visual and audio beep
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;Enable display-line-numbers-mode for emacs26
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode t)
  (global-linum-mode t)
  )

;; (setq linum-format "%d ")

;; Change font size
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;;Set C-v and M-v to scroll half page


;;Scrolling

(require 'smooth-scrolling)
(smooth-scrolling-mode t)
(setq mouse-wheel-progressive-speed nil)


;; disable startup screen
(setq inhibit-startup-screen t)
(fringe-mode 10)

;;Always enable paranthes match
(show-paren-mode t)


;; Might not show all contents
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 128))


;; Do not close emacs after pressing the close button
;; Directly copied from frame.el but now hide Emacs instead of killing
;; it when last frame will be closed.
;; Attributes to https://lists.gnu.org/archive/html/help-gnu-emacs/2016-01/msg00236.html
(defun handle-delete-frame-without-kill-emacs (event)
  "Handle delete-frame events from the X server."
  (interactive "e")
  (let ((frame (posn-window (event-start event)))
        (i 0)
        (tail (frame-list)))
    (while tail
      (and (frame-visible-p (car tail))
           (not (eq (car tail) frame))
           (setq i (1+ i)))
      (setq tail (cdr tail)))
    (if (> i 0)
        (delete-frame frame t)
      ;; Not (save-buffers-kill-emacs) but instead:
      (ns-do-hide-emacs))))

(when (eq system-type 'darwin)
  (advice-add 'handle-delete-frame :override
              #'handle-delete-frame-without-kill-emacs))
