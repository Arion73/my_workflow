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

;; delete current buffer and file
(defun delete-current-buffer-and-file()
  "Remove file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	(buffer (current-buffer))
	(name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure to remove this file?")
	(delete-file filename)
	(kill-buffer buffer)
	(message "File '%s' successfully removed." filename)))))

;; rename buffer and file
(defun rename-buffer-and-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))



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
  (kbd "SPC bR") 'rename-buffer-and-file
  (kbd "SPC be") 'eval-buffer
  (kbd "SPC yi") 'yas-insert-snippet
  (kbd "SPC ff") 'counsel-find-file
  (kbd "SPC qq") 'save-buffers-kill-emacs
  (kbd "SPC fs") 'save-buffer
  (kbd "SPC fr") 'counsel-recentf
  (kbd "SPC fD") 'delete-current-buffer-and-file
  (kbd "SPC gs") 'magit-status
  (kbd "SPC hdf") 'describe-function
  (kbd "SPC hdk") 'describe-key
  (kbd "SPC hdv") 'describe-variable
  (kbd "SPC hdm") 'describe-mode
  (kbd "SPC ps") 'projectile-switch-project
  (kbd "SPC td") 'org-todo
  (kbd "SPC Ts") 'counsel-load-theme
  (kbd "SPC wn") 'evil-window-next
  )

(which-key-add-key-based-replacements "SPC b" "buffer")
(which-key-add-key-based-replacements "SPC f" "file")
(which-key-add-key-based-replacements "SPC g" "magit")
(which-key-add-key-based-replacements "SPC h" "help")
(which-key-add-key-based-replacements "SPC hd" "describe")
(which-key-add-key-based-replacements "SPC p" "project")
(which-key-add-key-based-replacements "SPC q" "exit")
(which-key-add-key-based-replacements "SPC t" "toggle")
(which-key-add-key-based-replacements "SPC T" "theme")
(which-key-add-key-based-replacements "SPC w" "window")
(which-key-add-key-based-replacements "SPC y" "yasnippet")

(provide 'core-key-bindings)
;;; core-key-bindings.el ends here
