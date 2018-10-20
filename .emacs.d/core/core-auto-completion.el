;;; core-auto-completion.el --- Provide auto-completion configurations.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide auto-completion configurations.

;;; Code:


;; ivy
(use-package ivy
  :ensure
  :diminish ""
  :config
  (ivy-mode t)
  (setq ivy-height 10)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order)))
  (setq ivy-count-format "(%d/%d) ")
  (setq projectile-completion-system 'ivy))

;; counsel
(use-package counsel
  :ensure t
  :after (ivy)
  :diminish ""
  :config
  (counsel-mode t))

;; smex
(use-package smex
  :ensure t
  :diminish ""
  :config
  (smex-initialize))

;; auto-complete
(use-package auto-complete
  :ensure
  :diminish ""
  :config
  (auto-complete-mode t)
  (ac-config-default))

;; company-statistics
(use-package company-statistics
  :ensure t
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))

;; which-key
(use-package which-key
  :ensure t
  :defer t
  :hook (after-init-hook)
  :diminish (which-key-mode . "")
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-setup-minibuffer)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (setq which-key-idle-delay 0.01
	which-key-idle-secondary-delay 0.01
	which-key-echo-keystrokes 0.005)
  (setq which-key-sort-order 'which-key-key-order-alpha))

;; yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :hook (after-init-hook)
  :diminish ""
  :config
    (setq yas-snippet-dirs
	    '("~/.emacs.d/private/snippets/"      ;; personal snippets
	      "~/.emacs.d/elpa/yasnippet-snippets-20180909.1015/snippets/"))
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (yas-global-mode 1))

;; yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after (yasnippet))


(provide 'core-auto-completion)
;;; core-auto-completion.el ends here
