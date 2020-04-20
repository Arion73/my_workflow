;;; core-os.el --- Specific configurations for different OS.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Specific configurations for different OS.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAC OSX specific settings

(when (memq system-type '(darwin))
  (setq-default ns-use-srgb-colorspace nil)

  ;; In my env case (mac OSX, terminal), <Backspace> fails to delete. Below solves this problem.
  (normal-erase-is-backspace-mode 0)

  ;; dired-use-ls-dired problem fix
  (setq dired-use-ls-dired nil)

  ;; use OS X clipboard from terminal emacs
  (use-package osx-clipboard
    :defer t
    :diminish ""
    :config
    (osx-clipboard-mode +1))

  ;; ensure environment variables inside Emacs look the same in the shell for OSX
  (use-package exec-path-from-shell
    :config
    (add-hook 'after-init-hook
	      (lambda ()
		(exec-path-from-shell-initialize))))
  ;; Another method is to specify emacs_bash_profile.sh as the emacs shell env and
  ;; call it in ~/.bashrc


  ;; show file in Finder
  (use-package reveal-in-osx-finder
    :defer t
    :diminish "")
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Linux specific settings

;(when (memq system-type '(gnu/linux gnu/kfreebsd))
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Both Mac OSX and Linux specific settings

(when (memq system-type '(darwin gnu/linux gnu/kfreebsd))
  ;; set shell name
  (setq explicit-shell-file-name "/bin/bash")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Windows specific settings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-os)
;;; core-os.el ends here
