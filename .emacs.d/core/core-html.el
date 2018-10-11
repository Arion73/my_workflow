;;; core-html.el --- Provides functions for better support of .html file.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for Org-mode.

;;; Code:


;; htmlize --- syntax highlighting in exported HTML page
(use-package htmlize
  :ensure t
  :hook (org-mode-hook html-mode-hook)
  :defer t)

;; impatient-mode --- see the effect of HTML as you type it
(use-package impatient-mode
  :ensure t
  :hook (html-mode-hook)
  :defer t)

(provide 'core-html)
;;; core-html.el ends here
