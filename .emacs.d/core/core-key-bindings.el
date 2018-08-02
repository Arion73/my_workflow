;;; core-key-binding.el

;; Author: Zewei Wang

;; This file is not part of GNU Emacs.

;; Commentary:

;; set key-bindings as spacemacs

;; Code:

;; general
(use-package general
  :ensure t
  :diminish ""
  :config
  (general-evil-setup t)
  ;; define emacs leader key
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   "SPC" '(execute-extended-command :which-key "M-x")
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "bb" 'evil-switch-to-windows-last-buffer
   "bd" '(kill-buffer :which-key "kill-buffer")
   "bk" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
   "cl" '(comment-line :which-key "comment-line")
   "qq" '(save-buffers-kill-emacs :which-key "C-x C-c")
   "ff" '(counsel-find-file :which-key "find file")
   "fr" '(counsel-recentf :which-key "recent files")
   "g" '(:igonre t :which-key "Git")
   "gs" '(magit-status :which-key "git status")
   "hdf" '(describe-function :which-key "describe-function")
   "hdk" '(describe-key :which-key "describe-key")
   "hdv" '(describe-variable :which-key "describe-variable")
   "hdm" '(describe-mode :which-key "describe-mode")
   "td" '(org-todo :which-key "org-tod")
   "o" '(evil-window-next :which-key "evil-window-next")
   "1" '(delete-other-windows :which-key "delete-other-windows")
   "2" '(split-window-below :which-key "split-window-below")
   "3" '(split-window-right :which-key "split-window-right")
 )
  ;; define my leader key
  (general-define-key
   :states '(normal)
   :prefix ","
   "eb" '(eval-buffer :which-key "eval-buffer")
   "sl" '(set-solarized-light :which-key "set bg light")
   "sd" '(set-solarized-dark :which-key "set bg dark")
   "ml" '(markdown-live-preview-mode :which-key "markdown-live-preview-mode")
   "p" '(run-python :which-key "run-python")
   "cc" '(python-shell-send-buffer :which-key "python-shell-send-buffer")))


;; key-chord
(use-package key-chord
  :ensure
  :diminish ""
  :config
  ;; Max time delay between two key presses to be considered a key chord
	(setq key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
  (key-chord-mode t))

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


(provide 'core-key-bindings)
