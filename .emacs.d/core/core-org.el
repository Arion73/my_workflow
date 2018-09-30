;;; core-org.el --- Configurations for Org-mode.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for Org-mode.

;;; Code:

(add-hook 'org-mode-hook
	  (lambda ()
	    ;; org startup not folded
	    (setq org-inhibit-startup-visibility-stuff t)

	    ;; org-indent-mode
	    (setq org-startup-indented t)

	    ;; set underscore as _, subscript as _{}.
	    (setq org-export-with-sub-superscripts (quote {})) 

	    (setq org-src-fontify-natively t
                  org-src-preserve-indentation nil
                  org-src-tab-acts-natively t)

	    ;; inline display image
	    (setq org-startup-with-inline-images t)

	    ;;if there is a #+ATTR.*: width="200", resize to 200, otherwise resize to 400
	    (setq org-image-actual-width '(400))

	    ;;; display/update images in the buffer after I evaluate
	    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

	    ;;don't prompt me to confirm everytime I want to evaluate a block
	    (setq org-confirm-babel-evaluate nil)

	    ;; active Babel languages
	    (org-babel-do-load-languages 'org-babel-load-languages
					'((emacs-lisp . t)
					(python . t)
					(ipython . t)
					(C . t)
					(shell . t)
					(org . t)))))

;; htmlize --- syntax highlighting in exported HTML page
(use-package htmlize
  :ensure t
  :hook (org-mode-hook)
  :defer t)

;; org-bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org-present
(use-package org-present
  :ensure t
  :hook (org-mode-hook)
  :config
  (eval-after-load "org-present"
    '(progn
       (add-hook 'org-present-mode-hook
                 (lambda ()
                   (org-present-big)
                   (org-display-inline-images)
                   (org-present-hide-cursor)
                   (org-present-read-only)))
       (add-hook 'org-present-mode-quit-hook
                 (lambda ()
                   (org-present-small)
                   (org-remove-inline-images)
                   (org-present-show-cursor)
                   (org-present-read-write))))))

;; org-download --- Insertion of images
(use-package org-download
  :ensure t
  :hook (org-mode-hook))

;; org-projectile
(use-package org-projectile
  :ensure t
  :hook (org-mode-hook)
  :config
  (progn
    (setq org-projectile-projects-file
          "/Users/zwwang/Documents/myproject/mynotebook/todo.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; ob-ipython
(use-package ob-ipython
  :ensure t
  :hook (org-mode-hook)
  :config
  (add-to-list 'company-backends 'company-ob-ipython))


;; export org to html file when save buffer
(defun toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))
(add-hook 'org-mode-hook #'toggle-org-html-export-on-save)
(add-hook 'org-mode-hook
	  (lambda ()
	    (evil-define-key '(normal visual motion) org-mode-map
	      (kbd "SPC to") 'toggle-org-html-export-on-save)))


 ;; my css files
(defvar org-css-dir "~/.emacs.d/private/org-css/")

(defun toggle-org-custom-inline-style ()
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
  (interactive)
  (let* ((cssdir org-css-dir)
         (css-choices (directory-files cssdir nil ".css$"))
         (css (completing-read "theme: " css-choices nil t)))
    (concat cssdir css)))
 
(defun set-org-html-style (&optional backend)
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
        (message "Custom header file %s doesnt exist")))))

(provide 'core-org)
;;; core-org.el ends here
