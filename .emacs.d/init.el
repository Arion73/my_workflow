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
(package-refresh-contents)

;;;-----------------------------------------------------------------
;; my configurations
;;;-----------------------------------------------------------------

;; directory of all my configuration files
(add-to-list 'load-path "~/.emacs.d/core/")
;; evil
(require 'core-evil)
;; better defaults
(require 'core-better-defaults)
;; theme
(require 'core-theme)
;; mode line
(require 'core-mode-line)
;; key-bindings
(require 'core-key-bindings)
;; python
(require 'core-python)
;; markdown 
(require 'core-markdown)

;;-----------------------------------------------------
;;; package.el produced configuration automatically.
;;-----------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(custom-enabled-themes (quote (solarized)))
 '(custom-safe-themes
   (quote
    ("cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(electric-pair-mode t)
 '(fci-rule-color "#073642")
 '(frame-background-mode (quote light))
 '(package-archives
   (quote
    (("melpa-stable" . "http://stable.melpa.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (org-bullets org markdown-preview-mode window-numbering fancy-battery counsel use-package spaceline ein rainbow-delimiters auto-complete yasnippet gruvbox-theme which-key virtualenvwrapper zenburn-theme evil-indent-textobject evil-surround evil-leader evil markdown-mode color-theme-solarized)))
 '(recentf-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#475B61" :background "gray" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

(provide 'init)
;;; init.el ends here
