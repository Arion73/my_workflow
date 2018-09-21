;;; init.el --- My own configurations for Emacs.

;;; Author : Z.Wang
;;; Year : 2018

;;; Commentary:

;;; This is not a part of GNU Emacs.
;;; This is a private configuration file for my own Emacs.

;;; Code:

;;==========================================================
;; MY CONFIGURATIONS
;;==========================================================

(let ((file-name-handler-alist nil))

;; Make startup faster by redcing the frequency of garbage collection.
;; The default is 800000 bytes.
(setq gc-cons-threshold 30000000)
;; Make gc pauses faster by setting back to the default value.
(add-hook 'after-init-hook #'(lambda()
			       (setq gc-cons-threshold 800000)))

;;==========================================================
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;;==========================================================

;; directory of my core configuration files
(add-to-list 'load-path "~/.emacs.d/core/")
;; Note: Keep core-package near the top, as it needs to
;; count loaded packages number and time spent.
(require 'core-package)          ;; package management layer
(require 'core-basic)            ;; basic config 
(require 'core-evil)             ;; evil layer
(require 'core-python)           ;; python layer
(require 'core-c-c++)            ;; C/C++ layer
(require 'core-markdown)         ;; markdown layer
(require 'core-auto-completion)  ;; completion layer
(require 'core-global)           ;; package collections for global mode 
(require 'core-key-bindings)     ;; key-bindings
(require 'core-mode-line)        ;; mode-line
(require 'core-theme)            ;; load theme
)  ;; let ends here
;;
;;==========================================================
;; PACKAGE.EL AUTOMATICALLY PRODUCED CONFIGURATIONS
;;==========================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("04bff26d45b0ac4e64295eb5c73585e051a7d12cea771e54bb234d94a2ff59f8" "7b839cbaaf0f7da876cab50d745e65ee6a07a5c03edc8ae90defd33ab1af88ce" default)))
 '(package-selected-packages
   (quote
    (evil-lion highlight-parentheses diff-hl esup exec-path-from-shell pandoc-mode company-auctex auctex-latexmk ein ob-ipython org-projectile org-download org-present org-bullets markdown-preview-mode markdown-mode nose anaconda-mode virtualenvwrapper key-chord general window-numbering company-statistics auto-complete rainbow-delimiters which-key smex counsel osx-clipboard evil-matchit evil-indent-textobject evil-surround evil-leader evil yasnippet-snippets use-package py-autopep8 flycheck elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;==========================================================
(provide 'init)
;;; init.el ends here
