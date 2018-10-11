;;; core-latex.el --- Configurations for LaTex.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for LaTex.

;;; Code:

;; AUCTEX --- writting and formatting TEX file.
(use-package auctex
  :ensure t
  :hook (TeX-mode-hook)
  :defer t)

;; auctex-latexmk --- auto-build
(use-package auctex-latexmk
  :ensure t
  :hook (LaTeX-mode-hook)
  :config
   (auctex-latexmk-setup)
   (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; company-auctex --- auto-completion
(use-package company-auctex
  :ensure t
  :hook (LaTeX-mode-hook)
  :config
  (company-auctex-init))

;; company-math --- completion back-ends for math unicode sysmbols and latex tags
(use-package company-math
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(provide 'core-latex)
;;; core-latex.el ends here
