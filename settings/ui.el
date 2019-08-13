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
