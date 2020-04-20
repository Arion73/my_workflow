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

;; Init time start
(defvar my-emacs-start-time (current-time))

(let ((file-name-handler-alist nil))

;; Make startup faster by redcing the frequency of garbage collection.
;; The default is 800000 bytes.
(setq gc-cons-threshold 50000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS specific settings --- make experience consistency on different OS.
;; Set directory of my Emacs configurations files
(cond ((memq system-type '(darwin))
       ;; if you are using mac OSX.
      (message "* Emacs is running on mac OSX. *")
      (defconst my-emacs-directory (concat (getenv "HOME") "/.emacs.d/")))

      ((memq system-type '(gnu/linux gnu/kfreebsd))
       ;; if you are using Linx.
       (message "* Emacs is running on Linux. *")
       (defconst my-emacs-directory (concat (getenv "HOME") "/.emacs.d/")))

      ((memq system-type '(windows-nt ms-dos))
       ;; if you are using Windows.
       (message "* Emacs is running on Windows system. *"))

      (t (message "* Emacs is running on Unknown system. *")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directory of my core configuration files
(add-to-list 'load-path (concat my-emacs-directory "core"))
;; Note: Keep core-package at the top
(require 'core-package)          ;; package management layer
(require 'core-basic)            ;; basic Emacs configurations
(require 'core-gui)              ;; gui settings
(require 'core-os)               ;; MacOSX specific configurations

(require 'core-evil)             ;; evil layer
(require 'core-theme)            ;; load theme

(require 'core-completion)       ;; completion layer
(require 'core-global)           ;; package collections for global mode

(require 'core-org)              ;; org layer
(require 'core-org-html)         ;; org layer
(require 'core-org-latex)        ;; org layer
(require 'core-markdown)         ;; markdown layer
(require 'core-latex)            ;; latex layer
(require 'core-pdf)              ;; pdf layer

(require 'core-python)           ;; python layer
(require 'core-go)               ;; go layer
(require 'core-java)             ;; java layer
(require 'core-c-c++)            ;; C/C++ layer
(require 'core-html)             ;; HTML layer

(require 'core-key-bindings)     ;; key-bindings

(add-to-list 'load-path (concat my-emacs-directory "private"))
(require 'private-collections)   ;; private packages
)  ;; let ends here

(add-hook 'after-init-hook
            (lambda ()
	      (let ((loading-time (float-time (time-subtract (current-time) my-emacs-start-time))))
		(message "* Emacs initialized in %.2fs with %d packages loaded *" loading-time (length package-activated-list)))

	      ;; Make gc pauses faster by setting back to the default value.
	      (setq gc-cons-threshold 800000)))

;;==========================================================
;; PACKAGE.EL AUTOMATICALLY PRODUCED CONFIGURATIONS
;;==========================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "04bff26d45b0ac4e64295eb5c73585e051a7d12cea771e54bb234d94a2ff59f8" "7b839cbaaf0f7da876cab50d745e65ee6a07a5c03edc8ae90defd33ab1af88ce" default)))
 '(fci-rule-color "#62686E")
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   (quote
    (doom-themes doom-modeline reveal-in-osx-finder reveal-in-finder dockerfile-mode dockerfile docker google-c-style lv python-django flycheck-golangci-lint lsp-java lsp-ui company-lsp lsp-mode realgud autodisass-java-bytecode meghanada go-eldoc go-mode web-mode magit all-the-icons-ivy all-the-icons-dired all-the-icons company-quickhelp impatient-mode company-math org2ctex ob-async htmlize h2o all-the-icon evil-lion highlight-parentheses diff-hl esup exec-path-from-shell pandoc-mode company-auctex auctex-latexmk ob-ipython org-projectile org-download org-present org-bullets markdown-preview-mode markdown-mode nose anaconda-mode virtualenvwrapper key-chord general company-statistics rainbow-delimiters which-key smex counsel osx-clipboard evil-matchit evil-indent-textobject evil-surround evil-leader evil yasnippet-snippets use-package py-autopep8 flycheck elpy)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-c C-t")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 50)
 '(vc-annotate-background "#242730")
 '(vc-annotate-color-map
   (list
    (cons 20 "#7bc275")
    (cons 40 "#a6c677")
    (cons 60 "#d1ca79")
    (cons 80 "#FCCE7B")
    (cons 100 "#f4b96e")
    (cons 120 "#eda461")
    (cons 140 "#e69055")
    (cons 160 "#db8981")
    (cons 180 "#d082ae")
    (cons 200 "#C57BDB")
    (cons 220 "#d874b0")
    (cons 240 "#eb6d86")
    (cons 260 "#ff665c")
    (cons 280 "#d15e59")
    (cons 300 "#a35758")
    (cons 320 "#754f56")
    (cons 340 "#62686E")
    (cons 360 "#62686E")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "gray"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "brown"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "maroon")))))

;;==========================================================
(provide 'init)
;;; init.el ends here
