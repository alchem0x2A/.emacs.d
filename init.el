;; An Emacs init.el configuration makes the maximal use of use-package
;; Ideology:
;; - Custom defun OUTSIDE the scope of use-package blocks should be minimal
;; - Native Emacs keybindings should be respected (including the OS-related defaults)
;; - Personal defuns et should be provided as an separated package
;;   (with the help of vc-install-package)
;; - Minor modes that does not have clickable menu items will be put to delight / diminish
;; Necessary parts to enable use-package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization part
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
;; Switch to TUNA mirror if in GMT+8 zone
(let (
      (ctz (car (current-time-zone)))
      )
  
  (if (equal ctz 28800)
                                        ; GMT+8, use TUNA source
      (setq package-archives
            '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
              ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
              ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
              ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))
            )
                                        ; Fall-back to default 
    (setq package-archives
          '(("gnu" . "https://elpa.gnu.org/packages/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("melpa" . "https://melpa.org/packages/")
            ("org" . "https://orgmode.org/elpa/")))
    )
  )

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Support for the :vc keyword
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; :diminish and :delight keywords
(use-package diminish)
(use-package delight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin user settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global emacs settings.
(use-package emacs
  :bind
  (:map global-map
	;; s-l to select the current line as appear in other GUI text editors
	("s-l" . (lambda ()
		   (interactive)
		   (end-of-line)
		   (set-mark (line-beginning-position))
		   )))
  :config
  (setq gc-cons-threshold 100000000     ;100 MB
        read-process-output-max (* 1024 1024) ;1 MB
        inhibit-startup-screen t              ;disable startup
        visible-bell t                        ;enable visible bell
        ring-bell-function 'ignore
        custom-file (expand-file-name
                     "custom.el"
                     user-emacs-directory) ; Use ~/.emacs.d/custom.el for any custom
        )
  ;; Some built-in global modes 
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (size-indication-mode -1)
  (column-number-mode t)
  (global-display-line-numbers-mode t)
  (display-battery-mode t)
  (electric-pair-mode t)
  (show-paren-mode t)
  (global-hl-line-mode t)
  (save-place-mode t)
  (savehist-mode t)
  (recentf-mode t)
  (global-auto-revert-mode t)
  (winner-mode t)
  (require 'uniquify)
  
  ;; Set font
  (let ((my-default-ff "IBM Plex Mono"))
    (unless (member my-default-ff (font-family-list))
      (setq my-default-ff "Monospace")
      )
    (set-face-attribute 'default
			nil		;nil for all frames
			:family my-default-ff
			:height 120)
    )
  
  ;; Default to maximal upon startup
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; Unset some global super (⌘) bindings
  (unbind-key "s-p" global-map)
  (unbind-key "s-q" global-map)
  (unbind-key "s-o" global-map)
  (unbind-key "s-y" global-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part I: Editor-related packages
;; Packages to be included in this section deals with general-purpose text editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move text in natural behavior (M-Up etc)
(use-package move-text
  :config
  (move-text-default-bindings))

(use-package syntax-subword
  :config
  (global-syntax-subword-mode)
  )

(use-package expand-region
  :defer
  :bind
  (:map global-map
	("C-=" . er/expand-region)))

(use-package undo-tree
  :delight
  :init
  (global-undo-tree-mode)
  :bind
  (:map undo-tree-map
        ("s-Z" . undo-tree-redo)
	("s-z" . undo-tree-undo)
        ("C-x u" . undo-tree-visualize)))

(use-package crux
  :bind
  ;; Several key bindings that could be useful
  (:map global-map
	("s-<backspace>" . crux-kill-line-backwards)
	("s-<return>" . crux-smart-open-line)
	("C-x 4 t" . crux-transpose-windows)
	))

;; We always want gibberish!
(use-package lorem-ipsum)

;; Edit paren in LISP-related modes
;; Unfortunately I'm not that smart to master the behavior, especially conflict with
;; syntax-subword, so disable for now
(use-package paredit
  :disabled
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode))
  :config			    ;Unbind conflict keys in move-text
  (unbind-key "M-<up>" paredit-mode-map)
  (unbind-key "M-<down>" paredit-mode-map)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part II: UI-related packages
;; Packages to be included in this section interferes with the appearance
;; e.g. themes, tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hide-mode-line)

(use-package hl-todo
  :after (ef-themes)
  :delight
  :config
  (global-hl-todo-mode t)
  (setq hl-todo-keyword-faces
	`(("HOLD"   . ,(ef-themes-get-color-value 'prose-tag))
	  ("TODO"   . ,(ef-themes-get-color-value 'prose-todo))
	  ("OKAY"   . ,(ef-themes-get-color-value 'prose-done))
	  ("FAIL"   . ,(ef-themes-get-color-value 'err))
	  ("DONE"   . ,(ef-themes-get-color-value 'prose-done))
	  ("FIXME"  . ,(ef-themes-get-color-value 'prose-tag))
	  ("XXXX*"  . ,(ef-themes-get-color-value 'underline-err)))
	)
  )

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook
  (python-mode . rainbow-mode)
  (css-mode . rainbow-mode)
  (markdown-mode . rainbow-mode)
  (LaTeX-mode . rainbow-mode)
  (emacs-lisp-mode . rainbow-mode)
  )

(use-package eldoc
  :delight
  :ensure nil)

(use-package ef-themes
  :config
  (ef-themes-select 'ef-trio-light)
  ;; Use ef-themes-toggle to switch between light & dark
  (setq ef-themes-to-toggle '(ef-trio-light ef-trio-dark)))

(use-package visual-fill-column
  :delight
  :init
  (visual-fill-column-mode)
  :config
  (setq line-move-visual nil)
  (setq visual-fill-column-width 80))


;; Show a delayed hint about the key-bindings
(use-package which-key
  :delight
  :config
  (which-key-mode t)
  (which-key-setup-side-window-bottom)
  )

;;; The Recommended treemacs configs
;; We shall see how it works in the long term
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package ace-window
  :defer t
  :init
  ;; C-x o or M-o to switch window
  (global-set-key [remap other-window] 'ace-window)
  :bind
  (:map global-map
	("M-o" . ace-window)))

;; Move between windows in the same frame. Probably not as powerful as ace-window
(use-package windmove
  :bind
  (:map global-map
	("M-s-<up>" . windmove-up)
	("M-s-<down>" . windmove-down)
	("M-s-<left>" . windmove-left)
	("M-s-<right>" . windmove-right)
	) 
  )

(use-package super-save
  :after ace-window
  :defer
  :diminish
  :init
  (super-save-mode 1)
  (add-to-list 'super-save-triggers 'ace-window)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part III: System-related packages
;; Packages that interactives with system components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exec path from shell
;; Since my emacs setup are bound to a conda environment, copying
;; the CONDA_ env variables are encouraged

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-warn-duration-millis 2000) ;Much longer start-up expected for things like conda
  (dolist (var '(
		 "SSH_AUTH_SOCK"
		 "SSH_AGENT_PID"
		 "CONDA_PREFIX"
		 "CONDA_EXE"
		 "CONDA_PYTHON_EXE"
		 "LANG"
		 "LC_CTYPE"
		 "PYTHONPATH"
		 ))
  (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)
  )

;; Open GUI folder at place, OS-dependent
(use-package  reveal-in-folder)

;; External vterm support. Need libvterm.so
;; If fails to compile under ARM architecture, manual build is encouraged
(use-package vterm
  :defer
  :config
  (setq vterm-max-scrollback 5000
	vterm-kill-buffer-on-exit t
	)
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (message "Until I find a good solution, interactively toggling hl-line-mode will be used to disable the global-hl-line-mode temporarily")
	      (call-interactively 'hl-line-mode)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part IV: Search / completion packages
;; I'm stilling using a mixture between vertico / consult and ivy / counsel
;; whichever does not break should be encouraged
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minibuffer completion package
(use-package vertico
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Powerup for Emacs documentation
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package ivy
  :demand
  :delight
  :bind
  (:map ivy-minibuffer-map
        ("C-c i" . ivy-dispatching-done))
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
	ivy-height 15)
  )

(use-package counsel
  :defer
  :bind
  (:map global-map
	("M-x" . counsel-M-x))
  :config
  (setq ivy-re-builders-alist
	'((ivy-switch-buffer . ivy--regex-plus)
	  (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  
  )

(use-package swiper
  :defer
  :bind
  (:map global-map
	("C-s" . swiper))
  )

(use-package projectile
  :config
  (projectile-global-mode)
  :bind
  (:map projectile-mode-map
	("s-p" . projectile-command-map)))

(use-package flx)

(use-package flycheck)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part V: Programming modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :bind
  (("C-M-g" . magit-status)
   ("C-x g" . magit-dispatch)))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  (setq markdown-command "markdown"))


(use-package reftex)

(use-package tex
  :ensure auctex
  :after (reftex cdlatex)
  :init
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-PDF-mode t
	TeX-check-engine t)
  (setq-default TeX-master nil
		TeX-command-extra-options "-shell-escape")
  :hook
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . flyspell-ignore-tex)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . cdlatex-mode)
  (LaTeX-mode . TeX-source-correlate-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.pgf\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.tikz\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.pdf_tex\\'" . LaTeX-mode))
  (setq TeX-view-program-selection
	'((output-pdf "Skim")
	  (output-dvi "open")
	  (output-html "open")))
  (setq TeX-view-program-list
        '(("Skim" "displayline -b -g %n %o %b")))
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-source-correlate-start-server t)
  )

(use-package cdlatex)
(use-package reftex)

;; (use-package elpy
;;   :init
;;   (elpy-enable))

;;; Python-mode + anaconda mode
(use-package python-mode
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  :hook
  (python-mode . anaconda-mode)
  (python-mode . eldoc-mode)
  (python-mode . flycheck-mode)
  )

(use-package anaconda-mode
  :after python
  :defer
  :init
  (anaconda-mode t)
  )

(use-package py-isort)

(use-package conda
  :after (python exec-path-from-shell)
  :config
  (setq conda-anaconda-home (getenv "CONDA_PREFIX")
	conda-env-home-directory (getenv "CONDA_PREFIX"))
  (conda-env-initialize-interactive-shells)

  )

(use-package lua-mode)

(use-package yaml-mode)
(use-package jinja2-mode
  :mode "\\.j2$"
  :config
  (setq jinja2-enable-indent-on-save nil))
(use-package ansible
  :commands ansible-auto-decrypt-encrypt
  :hook
  (yaml-mode . ansible))

(use-package json-mode)
(use-package dockerfile-mode)
