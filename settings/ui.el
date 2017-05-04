
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
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;;Enable linum mode globally
(global-linum-mode t)
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 120))
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

;; font
(when (eq system-type 'darwin)

      ;; default Latin font (e.g. Consolas)
      (set-face-attribute 'default nil :family "MonacoB2")

      ;; default font size (point * 10)
      ;;
      ;; WARNING!  Depending on the default font,
      ;; if the size is not supported very well, the frame will be clipped
      ;; so that the beginning of the buffer may not be visible correctly. 
      (set-face-attribute 'default nil :height 120)

      ;; you may want to add different for other charset in this way.
      )

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 150))
