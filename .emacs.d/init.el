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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#003f8e"))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("04bff26d45b0ac4e64295eb5c73585e051a7d12cea771e54bb234d94a2ff59f8" "7b839cbaaf0f7da876cab50d745e65ee6a07a5c03edc8ae90defd33ab1af88ce" "cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" default)))
 '(display-battery-mode t)
 '(fci-rule-color "#003f8e")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (pandoc-mode company-auctex auctex-latexmk ein ob-ipython org-projectile org-download org-present org-bullets markdown-preview-mode markdown-mode nose anaconda-mode virtualenvwrapper key-chord general window-numbering yasnippet-snippets which-key use-package rainbow-delimiters py-autopep8 osx-clipboard flycheck evil-surround evil-matchit evil-leader evil-indent-textobject elpy counsel company-statistics auto-complete)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   (quote
    ((eval when
	   (fboundp
	    (quote rainbow-mode))
	   (rainbow-mode 1)))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff9da4")
     (40 . "#ffc58f")
     (60 . "#ffeead")
     (80 . "#d1f1a9")
     (100 . "#99ffff")
     (120 . "#bbdaff")
     (140 . "#ebbbff")
     (160 . "#ff9da4")
     (180 . "#ffc58f")
     (200 . "#ffeead")
     (220 . "#d1f1a9")
     (240 . "#99ffff")
     (260 . "#bbdaff")
     (280 . "#ebbbff")
     (300 . "#ff9da4")
     (320 . "#ffc58f")
     (340 . "#ffeead")
     (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(provide 'init)
;;; init.el ends here
