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

;; directory of my core configuration files
(add-to-list 'load-path "~/.emacs.d/core/")
;; Note: Keep core-package at the top
(require 'core-package)          ;; package management layer
(require 'core-basic)            ;; basic config 
(require 'core-evil)             ;; evil layer
(require 'core-theme)            ;; load theme

(require 'core-python)           ;; python layer
(require 'core-c-c++)            ;; C/C++ layer
(require 'core-html)             ;; HTML layer

(require 'core-org)              ;; org layer
(require 'core-org-html)         ;; org layer
(require 'core-org-latex)        ;; org layer
(require 'core-markdown)         ;; markdown layer
(require 'core-latex)            ;; latex layer
(require 'core-pdf)              ;; pdf layer

(require 'core-auto-completion)  ;; completion layer
(require 'core-global)           ;; package collections for global mode 
(require 'core-key-bindings)     ;; key-bindings
(require 'core-mode-line)        ;; mode-line
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
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "04bff26d45b0ac4e64295eb5c73585e051a7d12cea771e54bb234d94a2ff59f8" "7b839cbaaf0f7da876cab50d745e65ee6a07a5c03edc8ae90defd33ab1af88ce" default)))
 '(package-selected-packages
   (quote
    (impatient-mode company-math org2ctex ob-async htmlize h2o all-the-icon evil-lion highlight-parentheses diff-hl esup exec-path-from-shell pandoc-mode company-auctex auctex-latexmk ob-ipython org-projectile org-download org-present org-bullets markdown-preview-mode markdown-mode nose anaconda-mode virtualenvwrapper key-chord general window-numbering company-statistics auto-complete rainbow-delimiters which-key smex counsel osx-clipboard evil-matchit evil-indent-textobject evil-surround evil-leader evil yasnippet-snippets use-package py-autopep8 flycheck elpy)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-c C-t")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 50))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;==========================================================
(provide 'init)
;;; init.el ends here
