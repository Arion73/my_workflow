;;; core-html.el --- Provides functions for better support of .html file.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for Org-mode.

;;; Code:


;; htmlize --- syntax highlighting in exported HTML page
(use-package htmlize
  :hook (org-mode-hook html-mode-hook)
  :defer t)

;; impatient-mode --- see the effect of HTML as you type it
(use-package impatient-mode
  :hook (html-mode-hook)
  :defer t)


;; web-mode --- an autonomous emacs major-mode for editing web templates.
(add-to-list 'load-path (concat my-emacs-directory "private/"))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-engines-alist
	'(("django" . "\\.html\\'")
	  ("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(provide 'core-html)
;;; core-html.el ends here
