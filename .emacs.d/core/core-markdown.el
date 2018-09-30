;;; core-markdown.el --- Configurations for Markdown, jupyter notebook.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for Markdown, jupyter notebook.

;;; Code:


;; markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode markdown-live-preview-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

;; markdown preview
(use-package markdown-preview-mode
  :ensure t
  :hook (markdown-mode-hook))


(provide 'core-markdown)
;;; core-makrdown.el ends here
