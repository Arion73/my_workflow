;;; core-markdown.el

;; Author: Zewei Wang

;; This file is not part of GNU Emacs.

;; Commentary:

;; Configurations for Markdown, jupyter notebook and LeTax.

;; Code:

;;----------------------------------------------------------
;; markdown layer setting
;;__________________________________________________________

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode markdown-live-preview-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; markdown preview
(use-package markdown-preview-mode :ensure t)

;;----------------------------------------------------------
;; org layer setting
;;__________________________________________________________

;; org-bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org-present
(use-package org-present
  :ensure t
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
(use-package org-download :ensure t)

;; org-projectile
(use-package org-projectile
  :config
  (progn
    (setq org-projectile-projects-file
          "/Users/zwwang/Documents/myproject/mynotebook/todo.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)

;; org startup not folded
(setq org-inhibit-startup-visibility-stuff t)

;; ob-ipython
(use-package ob-ipython
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)
     ;; other languages..
     ))
  (setq org-confirm-babel-evaluate nil)   ;;don't prompt me to confirm everytime I want to evaluate a block
;;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

;; org-ipython --- convert an org-file to an ipynb
;; melpa does not have this package for now, you have to install it
;; manually

;;----------------------------------------------------------
;; jupyter notebook layer setting
;;__________________________________________________________

;; Emacs Ipython Notebook
(use-package ein
  :ensure t
  :config
	(require 'ein-loaddefs)
	(require 'ein-notebook)
	(require 'ein-subpackages)
	(setq ein:use-smartrep t)
	(setq ein:use-auto-complete t))

;;----------------------------------------------------------
;; latex layer setting
;;__________________________________________________________

;; auctex-latexmk --- auto-build
(use-package auctex-latexmk
  :ensure t
  :config
   (auctex-latexmk-setup)
   (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; company-auctex --- auto-completion
(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

;;----------------------------------------------------------
;; global setting
;;__________________________________________________________

;; pandoc-mode
(use-package pandoc-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'org-mode-hook 'pandoc-mode))




(provide 'core-markdown)
