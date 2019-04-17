;;; core-basic.el --- Basic configurations for my own Emacs.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; For a better experience of using Emacs generally, here I make some basic
;; default configurations.

;;; Code:


;; server
(require 'server)
(unless (server-running-p) (server-start))

;; Don't display startup messages
(setq initial-scratch-message "")
(setq inhibit-startup-message t)


;; Don't show menu-bar, scroll-bar, tool-bar in GUI Emacs
(menu-bar-mode 0)
;; emacs 27.0 does not have scroll-bar-mode
(scroll-bar-mode 0)
(tool-bar-mode 0)


(add-hook 'after-init-hook
	  (lambda()

	    ;; forbid warning sound when scroll out screen
	    (setq ring-bell-function 'ignore)

	    ;; set mouse scroll in terminal
	    (unless window-system
		;; enable mouse
		(xterm-mouse-mode t)
		(global-set-key [mouse-4] 'scroll-down-line)
		(global-set-key [mouse-5] 'scroll-up-line)
		;; set cursor scrolling speed
		(setq mouse-wheel-scroll-amount '(1)
		      mouse-wheel-progressive-speed nil)
		;; set cursor scrolling speed
		(setq scroll-margin 1
		      scroll-conservatively 101
		      scroll-up-aggressively 0.01
		      scroll-down-aggressively 0.01))

	    ;; fix cursor lag problem
	    (setq auto-window-vscroll nil)

	    ;; automatically save sessions
	    (setq desktop-buffers-not-to-save
			(concat "\\("
				"^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
				"\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
				"\\)$"))
	    (setq desktop-path (list my-emacs-directory))
	    (desktop-save-mode t)
	    (desktop-read)


	    ;; display line numbers
	    (global-display-line-numbers-mode t)


	    ;; electric-pair mode
	    (electric-pair-mode t)


	    ;; show-paren-mode
	    (show-paren-mode t)
	    ;; highlight-parentheses
	    (global-highlight-parentheses-mode t)


	    ;; Maximum line width
	    (setq-default fill-column 80)


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


	    ;; set tab behavior
	    (setq tab-always-indent 'complete)


	    ;; yes->y, no->n
	    (fset 'yes-or-no-p'y-or-n-p)


	    ;; Unicode
	    (setenv "LC_CTYPE" "UTF-8")
	    (setenv "LC_ALL" "en_US.UTF-8")
	    (setenv "LANG" "en_US.UTF-8")
	    (set-language-environment "UTF-8")


	    ;; Use utf-8 as default coding system
	    (prefer-coding-system 'utf-8)
	    (set-default-coding-systems 'utf-8)
	    (set-terminal-coding-system 'utf-8)
	    (set-charset-priority 'unicode)
	    (set-keyboard-coding-system 'utf-8)
	    (setq default-process-coding-system '(utf-8-unix . utf-8-unix))


	    ;; projectile mode
	    (projectile-mode t)


	    ;; flyspell mode
	    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
	    (add-hook 'text-mode-hook 'flyspell-prog-mode)


	    ;; auto-revert mode
	    (global-auto-revert-mode t)


	    ;; set terminal emacs clipboard
	    (setq select-enable-clipboard t)

	    ;; set shell name
	    (setq explicit-shell-file-name "/bin/bash")))



;; better whitespace mode setting
(defun better-whitespace ()
  "Toggle whitespace style."
  (interactive)
  (whitespace-mode -1)
  (let ((ws-small '(face tailing empty))
        (ws-big '(face tabs spaces trailing lines-tail space-before-tab
                       newline indentation empty space-after-tab space-mark
                       tab-mark newline-mark)))
    (if (eq whitespace-style ws-small)
        (progn
	  (setq whitespace-style ws-big)
          (message "whitespace-mode-full-on"))
	(setq whitespace-style ws-small)
        (message "whitespace-mode-small-on")))
  (whitespace-mode 1))

(add-hook 'after-init-hook
	  (lambda ()
	    ;; show trailing whitespace by whitespace mode
	    (setq whitespace-style '(face trailing empty))
	    (setq global-whitespace-mode t)
	    (global-set-key (kbd "C-c w") 'better-whitespace)

	    ;; delete trailing whitespace
	    (add-hook 'before-save-hook 'delete-trailing-whitespace)))


(defun set-no-process-query-on-exit ()
  "Prevent prompting a confirmation when closing buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(add-hook 'term-exec-hook 'set-no-process-query-on-exit)
(add-hook 'shell-mode-hook 'set-no-process-query-on-exit)
(add-hook 'compilation-mode-hook 'set-no-process-query-on-exit)
(add-hook 'inferior-python-mode-hook 'set-no-process-query-on-exit)


;; autosave buffer settings
(add-hook 'after-init-hook
	  (lambda ()
	    (auto-save-visited-mode t)
	    (setq-default auto-save-visited-interval 15)

	    (defadvice switch-to-buffer (before save-buffer-now activate)
	      "Save buffer when switch to other buffer."
	      (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))

	    (defadvice other-window (before other-window-now activate)
	      "Save buffer when switch to other window."
	      (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))

	    ;; save buffer on frame focus loss.
	    ;(add-hook 'focus-out-hook 'save-buffer)
	    (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
	    ))



(provide 'core-basic)
;;; core-basic.el ends here
