;;; core-osx.el --- Specific configurations for MacOSX.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Specific configurations for MacOSX.

;;; Code:


(add-hook 'after-init-hook
	  (lambda ()

	    (setq-default ns-use-srgb-colorspace nil)

	    ;; In my env case (mac OSX, terminal), <Backspace> fails to delete. Below solves this problem.
	    (normal-erase-is-backspace-mode 0)))


;; dired-use-ls-dired problem fix
(setq dired-use-ls-dired nil)


;; use OS X clipboard from terminal emacs
(use-package osx-clipboard
  :defer t
  :diminish ""
  :config
  (osx-clipboard-mode t))



(provide 'core-osx)
;;; core-osx.el ends here
