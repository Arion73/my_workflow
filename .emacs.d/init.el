;;; package --- Summary

;;; Author : Zewei Wang
;;; Year : 2018


;;; Commentary:

;;; This is a private configuration of Emacs by Zewei Wang.
;;; This configuration consists of 4 parts:
;;; 1. global editor configuration, e.g. linum-mode, color-theme
;;; 2. public packages' configuration, e.g. which-key
;;; 3. specific mode packages' configuration, e.g. evil, markdown
;;; 4. package.el produced configuration automatically.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;-----------------------------------------------------
;; 1. Editor configuration
;;-----------------------------------------------------

;; solution to that <Backspace> fails to delete
(normal-erase-is-backspace-mode 0)

;; Don't show startup messages
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; open linum-mode
(global-linum-mode t)

;; Don't show menu-bar, scroll-bar, tool-bar in GUI Emacs
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; forbid warning sound when scroll out screen
(setq ring-bell-function 'ignore)

;; set cursor scrolling speed
(setq mouse-wheel-progressive-speed nil)
(setq scroll-margin 1
    scroll-conservatively 1
    scroll-up-aggressively 0.01
    scroll-down-aggressively 0.01)

;; automatically save sessions
(setq desktop-buffers-not-to-save
	    (concat "\\("
		    "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
		    "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
		    "\\)$"))
(desktop-save-mode 1)

;; set default window(frame) size and position
(set-frame-position (selected-frame) 500 0)
(setq default-frame-alist '((height . 90) (width . 83)))

;; forbid cursor blinking
(blink-cursor-mode 0)

;; set font
(set-default-font "Source Code Pro-16")

;; yes->y, no->n
(fset 'yes-or-no-p'y-or-n-p)

;;; buit-in mode to show parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; set theme solarized background
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
;; Let the terminal decide the background color cause Emacs 24 has several color related bugs.
;(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))
(defun set-solarized-light ()
  (interactive)
  (customize-set-variable 'frame-background-mode 'light)
  (load-theme 'solarized t))
(defun set-solarized-dark ()
  (interactive)
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'solarized t))

;; set terminal emacs clipboard
;; The results are:
;; 1. copy/paste within Emacs is straightforward and fast.
;; 2. copy from other apps to Emacs: Ctrl+Shift+v
;; 3. copy from Emacs to other apps: mouse selection is now on X Selection, so right-click and copy shall copy the text into the Selection. Note that 'M-w" now won't copy anything into Selection or system clipboard.
(setq x-select-enable-clipboard t)

;; Unicode 
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

;;
(setq default-fill-column 80)

;; server
(require 'server)
(unless (server-running-p) (server-start))


;;-----------------------------------------------------
;;; 2. public packages' configuration, e.g. which-key
;;-----------------------------------------------------

;; which-key
(use-package which-key
  :ensure t
  :diminish ""
  :init
  (which-key-mode t)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha)
 )

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

;; auto-complete
(use-package auto-complete
  :ensure
  :config
  (ac-config-default))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure
  :config
	(set-face-attribute 'rainbow-delimiters-unmatched-face nil
						:inherit 'error
						:strike-through t)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; ivy
(use-package ivy
  :ensure
  :diminish (ivy-mode . "")
  :init (ivy-mode t)
  :bind
  :config
	(setq ivy-use-virtual-buffers t)
	(setq ivy-height 10)
	(setq ivy-initial-inputs-alist nil)
	(setq ivy-re-builders-alist
		  '((t . ivy--regex-ignore-order)))
	(setq ivy-count-format "(%d/%d) ")
)

;; counsel
(use-package counsel
  :ensure t)

;;-----------------------------------------------------
;;; 3. specific mode packages' configuration, e.g. evil, markdown
;;-----------------------------------------------------

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; evil-mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  ;; more configuration goes here
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))

;; indent-tabs
(add-hook 'python-mode-hook
		  (lambda()
			(setq tab-width 4)
			(set-variable 'python-indent-offset 4)
			))

;; apply the Evil h,j,k,l bindings to occur-mode-map when Emacs state
(add-hook 'occur-mode-hook
	  (lambda ()
	    (evil-add-hjkl-bindings occur-mode-map 'emacs
		(kbd "/") 'evil-search-forward
		(kbd "n") 'evil-search-next
		(kbd "N") 'evil-search-previous
		(kbd "C-f") 'evil-scroll-down
		(kbd "C-b") 'evil-scroll-up
		(kdb "C-w C-w") 'other-window)))

;; key-chord
(use-package key-chord
  :ensure
  :config
    ;; Max time delay between two key presses to be considered a key chord
	(setq key-chord-two-keys-delay 0.1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
    (key-chord-mode t))

;; Python virtualenv mode:
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location
        (expand-file-name "~/virtualenvs/")))

;; powerline
(use-package powerline
  :ensure
  :config
    (custom-set-faces
	'(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
	'(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
  
    (setq ns-use-srgb-colorspace nil)
    (setq evil-normal-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-visual-state-cursor '("gray" box))
    (setq evil-replace-state-cursor '("cyan" bar))
    (setq evil-operator-state-cursor '("gold" hollow))
    (setq evil-emacs-state-cursor '("purple" box))

  (powerline-center-evil-theme))

    (setq evil-normal-state-tag (propertize "NORMAL" 'face '((:foreground "DarkOrange" :background "#brown" :weight bold)))
	  evil-insert-state-tag (propertize "INSERT" 'face '((:foreground "red" :background "#666666" :weight bold)))
	  evil-visual-state-tag (propertize "VISUAL" 'face '((:foreground "gray" :background "#f9f9f9" :weight bold)))
	  evil-replace-state-tag (propertize "REPLACE" 'face '((:foreground "cyan" :background "#f9f9f9" :weight bold)))
	  evil-operator-state-tag (propertize "OPERATOR" 'face '((:foreground "gold" :background "#f9f9f9" :weight bold)))
	  evil-emacs-state-tag (propertize "EMACS" 'face '((:foreground "purple" :background "#f9f9f9" :weight bold))))
(cons 'evil-mode-line-tag mode-line-format)
(setq evil-mode-line-format '(before . mode-line-front-space))

;; Emacs Ipython Notebook
(use-package ein
  :ensure t
  :config
	(require 'ein-loaddefs)
	(require 'ein-notebook)
	(require 'ein-subpackages)
	(setq ein:use-smartrep t)
	;;(setq ein:jupyter-default-server-command "/Users/zwwang/.virtualenvs/all/bin/ipython")
	(setq ein:use-auto-complete t))

;; general
(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  ;; define emacs leader key
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   "SPC" '(execute-extended-command :which-key "M-x")
   "TAB" 'evil-switch-to-windows-last-buffer
   "bd" '(kill-buffer :which-key "kill-buffer")
   "bq" '(save-buffers-kill-emacs :which-key "C-x C-c")
   "eb" '(eval-buffer :which-key "eval-buffer")
   "ff" '(counsel-find-file :which-key "find file")
   "fr" '(counsel-recentf :which-key "recent files")
   "g" '(:igonre t :which-key "Git")
   "gs" '(magit-status :which-key "git status")
   "hf" '(describe-function :which-key "describe-function")
   "hk" '(describe-key :which-key "describe-key")
   "hv" '(describe-variable :which-key "describe-variable")
   "o" '(evil-window-next :which-key "evil-window-next")
   "1" '(delete-other-windows :which-key "delete-other-windows")
   "2" '(split-window-below :which-key "split-window-below")
   "3" '(split-window-right :which-key "split-window-right")
   "l" '(set-solarized-light :which-key "set bg light")
   "d" '(set-solarized-dark :which-key "set bg dark")
 )
  ;; define my leader key
  (general-define-key
   :states '(normal)
   :prefix ","
   "p" '(run-python :which-key "run-python")
   "cc" '(python-shell-send-buffer :which-key "python-shell-send-buffer"))

  )


;;-----------------------------------------------------
;;; 4. package.el produced configuration automatically.
;;-----------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(custom-enabled-themes (quote (solarized)))
 '(custom-safe-themes
   (quote
    ("3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(electric-pair-mode t)
 '(fci-rule-color "#073642")
 '(frame-background-mode (quote dark))
 '(package-archives
   (quote
    (("melpa-stable" . "http://stable.melpa.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (ein powerline rainbow-delimiters auto-complete yasnippet gruvbox-theme which-key virtualenvwrapper zenburn-theme evil-indent-textobject evil-surround evil-leader evil markdown-preview-mode markdown-mode use-package color-theme-solarized)))
 '(recentf-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "DarkOrange" :background "Black" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))


(provide 'init)
;;; init.el ends here
