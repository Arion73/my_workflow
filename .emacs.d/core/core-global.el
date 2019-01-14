;;; core-global.el --- Provide global packages configurations for all buffers.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some packages would be used in all buffers or several modes.
;; Here collects all these packages configurations.

;;; Code:


;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer t
  :diminish ""
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "gray"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "brown"))))
   '(rainbow-delimiters-unmatched-face ((t (:background "maroon")))))
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :strike-through t)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;; highlight-parentheses
(use-package highlight-parentheses
  :defer t
  :diminish ""
  :config
  (global-highlight-parentheses-mode))


;; view large file
(use-package vlf :defer t)


;; flycheck --- syntax checking
(use-package flycheck
  :defer t
  :config
  (global-flycheck-mode t))

(evil-define-key '(normal visual motion) global-map
  (kbd "SPC fl") 'flycheck-list-errors)


;; magit
(use-package magit
  :defer t
  :config
  (setq magit-refresh-status-buffer nil))

;; highlight version changes on the fringe.
(use-package diff-hl
  :defer t
  :config
  (global-diff-hl-mode t))


;; pandoc-mode
(use-package pandoc-mode
  :defer t
  :hook (markdown-mode-hook org-mode-hook))


;; dired-sidebar
(use-package dired-sidebar
  :defer t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  ;(setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; ibuffer-sidebar
(use-package ibuffer-sidebar
  :defer t
  :after (dired-sidebar)
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Helvetica" :height 150)))

(defun sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (ibuffer-sidebar-toggle-sidebar))

(evil-define-key '(normal visual motion) global-map
  (kbd "SPC ts") 'dired-sidebar-toggle-sidebar)
(evil-define-key '(normal visual motion) global-map
  (kbd "SPC tb") 'sidebar-toggle)

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


; novel
(use-package writeroom-mode)


(provide 'core-global)
;;; core-global.el ends here
