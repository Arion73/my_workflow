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


;; kill other buffer and window
(defun kill-other-buffer-and-window ()
  "Kill other buffer and window."
  (interactive)
  (other-window 1)
  (kill-buffer-and-window))


;; split window right and select next buffer
(defun next-buffer-right ()
  "Split window right and select next buffer."
  (interactive)
  (split-window-right)
  (other-window 1)
  (switch-to-next-buffer))


;; ansi-term
(defvar counter 0)
(defun my-ansi-term ()
  "My ansi-term."
  (interactive)
  (setq counter (+ counter 1))
  (let* ((default-directory (file-name-directory (buffer-file-name)))
	 (title "ansi-term")
	 ;(title (concat "ansi-term" (number-to-string counter)))
	 (buf-title (concat "*" title "*"))
	 ;(base-title "*ansi-term*")
	 ;(new-buf-title (concat base-title "<" (number-to-string counter) ">"))
	 )
    (set-buffer (make-term title explicit-shell-file-name))
    (term-mode)
    (term-char-mode)
    (if (not (get-buffer-window buf-title 'visible))
	(progn
	  (split-window-below)
	  (if (<= (* 2 (window-height)) (frame-height))
	      (enlarge-window 2))
	  (other-window 1)
	  (switch-to-buffer buf-title)
	  ;(rename-buffer new-buf-title)
	  )
      (other-window 1)
      ;(rename-buffer new-buf-title)
      )))


(require 'evil)
;; global key bindings
(evil-define-key '(normal visual motion) global-map
  (kbd "SPC SPC") 'counsel-M-x
  (kbd "SPC TAB") 'switch-to-prev-buffer
  (kbd "SPC !") 'shell-command
  (kbd "SPC '") 'shell
  (kbd "SPC /") 'my-ansi-term
  (kbd "SPC 0") 'delete-window
  (kbd "SPC 1") 'delete-other-windows
  (kbd "SPC 2") 'split-window-below
  (kbd "SPC 3") 'split-window-right
  (kbd "SPC bb") 'ivy-switch-buffer
  (kbd "SPC bd") 'kill-buffer
  (kbd "SPC bk") 'kill-buffer-and-window
  (kbd "SPC bo") 'kill-other-buffer-and-window
  (kbd "SPC bm") 'kill-all-buffers
  (kbd "SPC bR") 'rename-buffer-and-file
  (kbd "SPC be") 'eval-buffer
  (kbd "SPC bv") 'next-buffer-right
  (kbd "SPC yi") 'yas-insert-snippet
  (kbd "SPC ff") 'counsel-find-file
  (kbd "SPC qq") 'save-buffers-kill-emacs
  (kbd "SPC fs") 'save-buffer
  (kbd "SPC fr") 'counsel-recentf
  (kbd "SPC fD") 'delete-current-buffer-and-file
  (kbd "SPC gs") 'magit-status
  (kbd "SPC hf") 'describe-function
  (kbd "SPC hk") 'describe-key
  (kbd "SPC hv") 'describe-variable
  (kbd "SPC hm") 'describe-mode
  (kbd "SPC pa") 'projectile-add-known-project
  (kbd "SPC ps") 'projectile-switch-project
  (kbd "SPC pf") 'projectile-find-file
  (kbd "SPC Ts") 'counsel-load-theme
  (kbd "SPC wb") 'browse-web
  (kbd "SPC we") 'eww-browse-with-external-browser
  (kbd "SPC wn") 'evil-window-next
  )

(which-key-add-key-based-replacements "SPC b" "buffer")
(which-key-add-key-based-replacements "SPC f" "file")
(which-key-add-key-based-replacements "SPC g" "magit")
(which-key-add-key-based-replacements "SPC h" "help")
(which-key-add-key-based-replacements "SPC p" "project")
(which-key-add-key-based-replacements "SPC q" "exit")
(which-key-add-key-based-replacements "SPC t" "toggle")
(which-key-add-key-based-replacements "SPC T" "theme")
(which-key-add-key-based-replacements "SPC w" "window")
(which-key-add-key-based-replacements "SPC y" "yasnippet")

(evil-define-key '(insert) global-map
  (kbd "C-e") 'move-end-of-line
  (kbd "C-a") 'move-beginning-of-line)

(which-key-add-key-based-replacements "C-c !" "flycheck")
(which-key-add-key-based-replacements "C-c &" "yasnippet")


(provide 'core-key-bindings)
;;; core-key-bindings.el ends here
