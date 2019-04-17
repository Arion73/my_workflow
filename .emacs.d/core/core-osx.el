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


;; ensure environment variables inside Emacs look the same in the shell
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :defer t
  :config
  (exec-path-from-shell-initialize))


;; close emacs gui window without kill emacs server
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (switch-to-buffer "*scratch*")
  (save-some-buffers t)
  (let ((frame   (posn-window (event-start event)))
        (numfrs  (length (frame-list))))
    (if (> numfrs 1)
      ad-do-it
      (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))

(defun hide-emacs()
  "Hide Emacs."
  (interactive)
  (switch-to-buffer "*scratch*")
  (save-some-buffers t)
  (condition-case nil (delete-frame)
    (error
     (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false")))
  )

(evil-define-key  '(normal visual motion) global-map
  (kbd "SPC q") 'hide-emacs)

(which-key-add-key-based-replacements "SPC q" "hide-emacs")

;; 开机启动后自动隐藏
(add-hook 'after-init-hook 'hide-emacs)



(provide 'core-osx)
;;; core-osx.el ends here
