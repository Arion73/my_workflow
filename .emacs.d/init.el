;;; package --- Summary

;;; Author : Zewei Wang
;;; Year : 2018


;;; Commentary:

;;; This is a private configuration for my Emacs.

;;; Code:
(require 'package)
;; add melpa stable
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; add melpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;;;-----------------------------------------------------------------
;; my configurations
;;;-----------------------------------------------------------------

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
  '(
    ;; better defaults
    yasnippet-snippets
    ;; python layer
    elpy
    flycheck  ;; realtime syntax checking
    py-autopep8
    ))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      my-packages)

;; directory of all my configuration files
(add-to-list 'load-path "~/.emacs.d/core/")

;;==========================================================
;; evil
;;==========================================================
(require 'core-evil)

;;==========================================================
;; better defaults
;;==========================================================
(savehist-mode 1)
(require 'core-better-defaults)

;; yasnippet
(setq yas-snippet-dirs
	'("~/.spacemacs.d/private/snippets"      ;; personal snippets
	"~/.spacemacs.d/elpa/yasnippet-snippets-20180714.1322/snippets"))
;;==========================================================
;; theme
;;==========================================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/private/zelin-theme")
(setq ns-use-srgb-colorspace nil)

(if (display-graphic-p)
    (progn
      ;; if graphic
      (load-theme 'zelin-dark-02-gui))
  ;; else
  (load-theme 'zelin-dark-02-terminal))
;;-----------------------------------------------------------
;; mode line
;;-----------------------------------------------------------
(require 'core-mode-line)

;;-----------------------------------------------------------
;; key-bindings
;;-----------------------------------------------------------
(require 'core-key-bindings)

;;-----------------------------------------------------------
;; python
;;-----------------------------------------------------------
(require 'core-python)

;; elpy
(elpy-enable)
;; highlight-indentation is a dependency package. Do not display highlight-indentation as default
(add-hook 'elpy-mode-hook (lambda() (highlight-indentation-mode -1)))

;; flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; py-autopep8
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;-----------------------------------------------------------
;; markdown 
;;-----------------------------------------------------------
(require 'core-markdown)

;;-----------------------------------------------------
;;; package.el produced configuration automatically.
;;-----------------------------------------------------

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
    (pandoc-mode company-auctex auctex-latexmk ein ob-ipython org-projectile org-download org-present org-bullets markdown-preview-mode markdown-mode nose anaconda-mode virtualenvwrapper key-chord general window-numbering company-statistics auto-complete rainbow-delimiters which-key smex counsel osx-clipboard evil-matchit evil-indent-textobject evil-surround evil-leader evil yasnippet-snippets use-package py-autopep8 flycheck elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'init)
;;; init.el ends here
