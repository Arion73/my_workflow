;;; core-basic.el --- Basic configurations for my own Emacs.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; For a better experience of using Emacs generally, here I make some basic
;; default configurations.

;;; Code:


;; Don't display startup messages
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Don't show menu-bar, scroll-bar, tool-bar in GUI Emacs
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(add-hook 'after-init-hook
	  (lambda()
	    ;; forbid warning sound when scroll out screen
	    (setq ring-bell-function 'ignore)

	    ;; In my env case (mac OSX, terminal), <Backspace> fails to delete. Below solves this problem.
	    (normal-erase-is-backspace-mode 0)

	    ;; set mouse scroll in terminal
	    (unless window-system
		;; enable mouse
		(xterm-mouse-mode t)
		(global-set-key [mouse-4] 'scroll-down-line)
		(global-set-key [mouse-5] 'scroll-up-line)
		;; set cursor scrolling speed
		(setq mouse-wheel-progressive-speed nil)
		;; set cursor scrolling speed
		(setq scroll-margin 1
		scroll-conservatively 0
		scroll-up-aggressively 0.01
		scroll-down-aggressively 0.01))

	    ;; automatically save sessions
	    (setq desktop-buffers-not-to-save
			(concat "\\("
				"^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
				"\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
				"\\)$"))
	    (setq desktop-path '("~/.emacs.d/"))
	    (desktop-save-mode t)
	    (desktop-read)

	    ;; display line numbers
	    (global-display-line-numbers-mode t)

	    ;; electric-pair mode
	    (electric-pair-mode t)

	    ;; show-paren-mode
	    (show-paren-mode t)

	    ;; Maximum line width
	    (setq default-fill-column 80)

	    ;; wrap lines, visual line mode
	    (global-visual-line-mode 1)

	    ;; save minibuffer history
	    (savehist-mode 1)

	    ;; forbid cursor blinking
	    (blink-cursor-mode 0)

	    ;; ido-mode
	    (setq ido-enable-fex-matching t)
	    (setq ido-everywhere t)
	    (ido-mode 1)

	    ;; set font
	    (set-frame-font "Source Code Pro-15")

	    ;; yes->y, no->n
	    (fset 'yes-or-no-p'y-or-n-p)

	    ;; Unicode
	    (setenv "LC_CTYPE" "UTF-8")
	    (setenv "LC_ALL" "en_US.UTF-8")
	    (setenv "LANG" "en_US.UTF-8")
	    (set-language-environment "UTF-8")

	    ;; Use utf-8 as default coding system
	    (prefer-coding-system 'utf-8)                                 
	    (set-charset-priority 'unicode)                               
	    (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

	    ;; projectile mode
	    (projectile-mode t)

	    ;; flyspell mode
	    (add-hook 'prog-mode-hook 'flyspell-prog-mode)

	    ;; auto-revert mode
	    (global-auto-revert-mode t)))

;; server
(require 'server)
(unless (server-running-p) (server-start))

;; set terminal emacs clipboard
(setq select-enable-clipboard t)
;; use OS X clipboard from terminal emacs
(use-package osx-clipboard
  :ensure t
  :defer t
  :diminish ""
  :config
  (osx-clipboard-mode t))


(provide 'core-basic)
;;; core-basic.el ends here
