;;; core-org-html.el --- Provides functions for better support of exporting org-mode file to .html file.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for Org-mode.

;;; Code:


(defun toggle-org-html-export-on-save ()
  "Export org file to html when saving buffer."
  (interactive)
  (if (memq 'org-export-and-move-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-export-and-move-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-export-and-move-html nil t)
    (message "Enabled org html export on save for current buffer...")))

(defun org-export-and-move-html ()
  "Org exports to html, move html file to specific directory."
  (interactive)
  (org-html-export-to-html)
  (let ((filename (concat (file-name-sans-extension buffer-file-name) ".html"))
	(dest-dir "~/documents/myproject/mynotebook/html/"))
    (if (not (and filename (file-exists-p filename)))
	(message "%s do not exists!" filename)
      (shell-command (concat "mv " filename " " dest-dir))
      (delete-file filename)
      (message "%s saved!" (concat dest-dir (file-name-nondirectory filename))))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (evil-define-key '(normal visual motion) org-mode-map
	      (kbd "SPC toh") 'toggle-org-html-export-on-save)
	    (evil-define-key '(normal visual motion) org-mode-map
	      (kbd "SPC oh") 'org-export-and-move-html)
	    (evil-define-key '(normal visual motion) org-mode-map
	      (kbd "SPC ov") 'org-html-export-to-html)))


;; my css files
(defvar org-css-dir (concat my-emacs-directory "private/org-css/"))

;; Select css file for html
(defun toggle-org-custom-inline-style ()
  "Toggle selecting css file when exporting to html."
  (interactive)
  (let ((hook 'org-export-before-parsing-hook)
        (fun 'set-org-html-style))
    (if (memq fun (eval hook))
        (progn
          (remove-hook hook fun 'buffer-local)
          (message "Removed %s from %s" (symbol-name fun) (symbol-name hook)))
      (add-hook hook fun nil 'buffer-local)
      (message "Added %s to %s" (symbol-name fun) (symbol-name hook)))))

(defun org-theme ()
  "Select css file."
  (interactive)
  (let* ((cssdir org-css-dir)
         (css-choices (directory-files cssdir nil ".css$"))
         (css (completing-read "theme: " css-choices nil t)))
    (concat cssdir css)))

(defun set-org-html-style (&optional backend)
  "Insert css into html if backend is html.
BACKEND: html."
  (interactive)
  (when (or (null backend) (eq backend 'html))
    (let ((f (or (and (boundp 'org-theme-css) org-theme-css) (org-theme))))
      (if (file-exists-p f)
          (progn
            (set (make-local-variable 'org-theme-css) f)
            (set (make-local-variable 'org-html-head)
                 (with-temp-buffer
                   (insert "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n")
                   (insert-file-contents f)
                   (goto-char (point-max))
                   (insert "\n/*]]>*/-->\n</style>\n")
                   (buffer-string)))
            (set (make-local-variable 'org-html-head-include-default-style)
                 nil)
            (message "Set custom style from %s" f))
        (message "Custom header file %s doesnt exist" f)))))


(provide 'core-org-html)
;;; core-org-html.el ends here
