;;; core-better-defaults.el

;; Author: Zewei Wang

;; This file is not part of GNU Emacs.

;; Commentary:

;; For a better experience of using Emacs generally, here I selected a few
;; packages to improve the default configurations.

;; Code:

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

;; set default window(frame) size
(setq default-frame-alist '((height . 90) (width . 83)))

;; forbid cursor blinking
(blink-cursor-mode 0)

;; set font
(set-default-font "Source Code Pro-15")

;; yes->y, no->n
(fset 'yes-or-no-p'y-or-n-p)

;; Unicode
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

;;
(setq default-fill-column 80)

;; server
(require 'server)
(unless (server-running-p) (server-start))

;; set terminal emacs clipboard
(setq x-select-enable-clipboard t)
;; use OS X clipboard from terminal emacs
(use-package osx-clipboard
  :ensure t
  :config
  (osx-clipboard-mode t))

;; ivy
(use-package ivy
  :ensure
  :diminish ""
  :init (ivy-mode t)
  :bind
  :config
	(setq ivy-height 10)
	(setq ivy-use-virtual-buffers t)
	(setq ivy-initial-inputs-alist nil)
	(setq ivy-re-builders-alist
		    '((t . ivy--regex-ignore-order)))
	(setq ivy-count-format "(%d/%d) ")
  )

;; counsel
(use-package counsel
  :ensure t
  :diminish "")


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

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure
  :diminish ""
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
						          :inherit 'error
						          :strike-through t)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish ""
  :config
    (add-to-list 'load-path
		"~/.emacs.d/plugins/yasnippet")
    (yas-global-mode 1)
    (define-key yas-minor-mode-map (kbd "<tab>") nil))

;; auto-complete
(use-package auto-complete
  :ensure
  :diminish ""
  :config
  (ac-config-default))

;; companty-statistics
(use-package company-statistics
  :ensure t
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))

;; header2
(add-to-list 'load-path "~/.emacs.d/private/header2")
(require 'header2)
(setq make-header-hook '(header-title
			 header-blank
			 header-file-name
			 header-description
			 header-author
			 header-copyright
			 header-creation-date
			 header-end-line
			 header-commentary
			 header-blank
			 header-end-line
			 header-code
			 header-eof))
;; To have Emacs update file headers automatically whenever you save a
;; file, put this in your init file:
(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)
;; To have Emacs add a file header whenever you create a new file in
;; some mode, put this in your init file:
(autoload 'auto-make-header "header2")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)


(provide 'core-better-defaults)
