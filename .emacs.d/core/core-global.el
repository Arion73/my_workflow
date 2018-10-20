;;; core-global.el --- Provide global packages configurations for all buffers.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some packages would be used in all buffers or several modes.
;; Here collects all these packages configurations.

;;; Code:


;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :diminish ""
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :strike-through t)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; highlight-parentheses
(use-package highlight-parentheses
  :ensure t
  :defer t
  :diminish ""
  :config
  (global-highlight-parentheses-mode))

;; view large file
(use-package vlf
  :ensure t
  :defer t)

;; ensure environment variables inside Emacs look the same in the shell
(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :defer t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; flycheck --- syntax checking
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

;; highlight version changes on the fringe.
(use-package diff-hl
  :ensure t
  :defer t
  :config
  (global-diff-hl-mode t))

;; pandoc-mode
(use-package pandoc-mode
  :ensure t
  :defer t
  :hook (markdown-mode-hook org-mode-hook))


(provide 'core-global)
;;; core-global.el ends here
