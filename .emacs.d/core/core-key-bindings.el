;;; core-key-binding.el --- provide key-bindings similar to  spacemacs's settings.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; set key-bindings similar to  spacemacs's settings

;;; Code:

;; kill all buffers
(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (dolist (cur (buffer-list))
    (kill-buffer cur)))


(require 'evil)

;; global key bindings
(evil-define-key '(normal visual motion) global-map
  (kbd "SPC SPC") 'counsel-M-x
  (kbd "SPC TAB") 'switch-to-prev-buffer
  (kbd "SPC !") 'shell-command
  (kbd "SPC '") 'shell
  (kbd "SPC 1") 'delete-other-windows
  (kbd "SPC bb") 'ivy-switch-buffer
  (kbd "SPC bd") 'kill-buffer
  (kbd "SPC bk") 'kill-buffer-and-window
  (kbd "SPC bm") 'kill-all-buffers
  (kbd "SPC bR") 'rename-buffer
  (kbd "SPC eb") 'eval-buffer
  (kbd "SPC yi") 'yas-insert-snippet
  (kbd "SPC ff") 'counsel-find-file
  (kbd "SPC qq") 'save-buffers-kill-emacs
  (kbd "SPC fs") 'save-buffer
  (kbd "SPC fr") 'counsel-recentf
  (kbd "SPC gs") 'magit-status
  (kbd "SPC hdf") 'describe-function
  (kbd "SPC hdk") 'describe-key
  (kbd "SPC hdv") 'describe-variable
  (kbd "SPC hdm") 'describe-mode
  (kbd "SPC td") 'org-todo
  (kbd "SPC Ts") 'counsel-load-theme
  (kbd "SPC o") 'evil-window-next
  )


(provide 'core-key-bindings)
;;; core-key-bindings.el ends here
