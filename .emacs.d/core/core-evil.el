;;; core-evil.el --- Configurations for Evil mode.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for Evil mode.

;;; Code:

;; evil-mode
(use-package evil
  :ensure t
  :defer t
  :config
  (evil-mode 1))

;; key-chord
(use-package key-chord
  :ensure t
  :after evil
  :diminish ""
  :config
  ;; Max time delay between two key presses to be considered a key chord
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map "ii" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "ii" 'evil-normal-state)
  (key-chord-define evil-motion-state-map "ii" 'evil-normal-state)
  (key-chord-mode t))


(use-package evil-surround
  :ensure t
  :hook (evil-mode-hook)
  :after (evil)
  :config
  (global-evil-surround-mode))

(use-package evil-indent-textobject
  :ensure t
  :hook (evil-mode-hook)
  :after (evil))


;; evil-matchit --- jump between blocks
(use-package evil-matchit
  :ensure t
  :hook (evil-mode-hook)
  :after (evil)
  :config
  (global-evil-matchit-mode 1))

;;evil-lion
(use-package evil-lion
  :ensure t
  :after (evil)
  :hook (evil-mode-hook)
  :config
  (evil-define-key '(normal visual) prog-mode-map
    (kbd "g l") 'evil-lion-left
    (kbd "g L") 'evil-lion-right)
  (evil-lion-mode))

;; apply the Evil h,j,k,l bindings to occur-mode-map when Emacs state
(add-hook 'occur-mode-hook
	        (lambda ()
	          (evil-add-hjkl-bindings occur-mode-map 'emacs
		          (kbd "/") 'evil-search-forward
		          (kbd "n") 'evil-search-next
		          (kbd "N") 'evil-search-previous
		          (kbd "C-f") 'evil-scroll-down
		          (kbd "C-b") 'evil-scroll-up
		          (kdb "C-w C-w") 'other-window)))



(provide 'core-evil)
;;; core-evil.el ends here
