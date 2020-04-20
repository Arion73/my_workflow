;;; core-evil.el --- Configurations for Evil mode.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for Evil mode.

;;; Code:

;; evil-mode
(use-package evil
  :config
  ;;(setq evil-default-state 'emacs)
  (evil-mode 1))

;; key-chord
(use-package key-chord
  :after evil
  :diminish ""
  :config
  ;; Max time delay between two key presses to be considered a key chord
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-motion-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-emacs-state-map   "jk" 'evil-normal-state)
  (key-chord-mode t))


(use-package evil-surround
  :hook (evil-mode-hook)
  :after (evil)
  :config
  (global-evil-surround-mode))

(use-package evil-indent-textobject
  :hook (evil-mode-hook)
  :after (evil))


;; evil-matchit --- jump between blocks
(use-package evil-matchit
  :hook (evil-mode-hook)
  :after (evil)
  :config
  (global-evil-matchit-mode 1))

;;evil-lion
(use-package evil-lion
  :after (evil)
  :config
  (evil-define-key '(normal visual) prog-mode-map
    (kbd "gl") 'evil-lion-left
    (kbd "gL") 'evil-lion-right)
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
