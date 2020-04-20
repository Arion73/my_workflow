;;; core-gui.el --- configuration for the theme and mode-line.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Themes: doom-themes
;; Mode-line: doom-modeline

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;                 ICONS               ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-dired
  :defer t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(defun dired-mode-setup ()
  "Fix font problem for icons of sidebar."
  (dired-hide-details-mode 1)
  (local-set-key (kbd "TAB") 'dired-subtree-cycle)
  (font-lock-mode 0))

(add-hook 'dired-mode-hook 'dired-mode-setup)

(use-package all-the-icons-ivy
  :config
  (if (display-graphic-p)
      (all-the-icons-ivy-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;                 SIDEBAR             ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; dired-sidebar
;(use-package dired-sidebar
  ;:defer t
  ;:bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  ;:commands (dired-sidebar-toggle-sidebar)
  ;:init
  ;(add-hook 'dired-sidebar-mode-hook
            ;(lambda ()
              ;(unless (file-remote-p default-directory)
                ;(auto-revert-mode))))
  ;:config
  ;(push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  ;(push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  ;;(setq dired-sidebar-theme 'vscode)
  ;(setq dired-sidebar-use-term-integration t)
  ;(setq dired-sidebar-use-custom-font t))
;
;
;;; ibuffer-sidebar
;(use-package ibuffer-sidebar
  ;:defer t
  ;:after (dired-sidebar)
  ;:commands (ibuffer-sidebar-toggle-sidebar)
  ;:config
  ;(setq ibuffer-sidebar-use-custom-font t)
  ;(setq ibuffer-sidebar-face `(:family "Helvetica" :height 150)))
;
;(defun sidebar-toggle ()
  ;"Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  ;(interactive)
  ;(ibuffer-sidebar-toggle-sidebar))
;
;(evil-define-key '(normal visual motion) global-map
  ;(kbd "SPC ts") 'dired-sidebar-toggle-sidebar)
;(evil-define-key '(normal visual motion) global-map
  ;(kbd "SPC tb") 'sidebar-toggle)

(use-package neotree
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;                 THEME               ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;                 MODE-LINE               ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'core-gui)
;;; core-gui.el ends here
