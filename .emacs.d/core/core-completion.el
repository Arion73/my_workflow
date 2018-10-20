;;; core-completion.el --- Provide auto-completion configurations.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide auto-completion configurations.

;;; Code:


;; which-key
(use-package which-key
  :ensure t
  :diminish (which-key-mode . "")
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-setup-minibuffer)
  (set-face-attribute
   'which-key-local-map-description-face nil :weight 'bold)
  (setq which-key-idle-delay 0.01
	which-key-idle-secondary-delay 0.01
	which-key-echo-keystrokes 0.005)
  (setq which-key-sort-order 'which-key-key-order-alpha))


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


;; company
(use-package company
  :ensure t
  :config
  (setq company-dabbrev-downcase 0
	company-idle-delay 0.5
	company-show-numbers t)
  (add-hook 'after-init-hook 'global-company-mode))

;; company quickhelp
(use-package company-quickhelp
  :ensure t
  :after (company)
  :config
  (setq company-quickhelp-delay nil)
  (eval-after-load 'company
    '(define-key company-active-map
       (kbd "M-h") #'company-quickhelp-manual-begin))
  (company-quickhelp-mode t))

;; company-statistics
(use-package company-statistics
  :ensure t
  :after (company)
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))


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
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (yas-global-mode 1))

;; yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after (yasnippet))


;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


;;; Based on:
;;; - https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
;;; - https://emacs.stackexchange.com/a/13290/12534
;;; - http://stackoverflow.com/a/22863701/3538165
;;;
;;; See also:
;;; - https://emacs.stackexchange.com/a/24800/12534
;;; - https://emacs.stackexchange.com/q/27459/12534

;; <return> is for windowed Emacs; RET is for terminal Emacs
(dolist (key '("<return>" "RET"))
  ;; Here we are using an advanced feature of define-key that lets
  ;; us pass an "extended menu item" instead of an interactive
  ;; function. Doing this allows RET to regain its usual
  ;; functionality when the user has not explicitly interacted with
  ;; Company.
  (define-key company-active-map (kbd key)
    `(menu-item nil company-complete
                :filter ,(lambda (cmd)
                           (when (company-explicit-action-p)
                             cmd)))))

(define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
;; Company appears to override the above keymap based on company-auto-complete-chars.
;; Turning it off ensures we have full control.
;(setq company-auto-complete-chars nil)


(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

(global-set-key (kbd "M-/") 'hippie-expand)


(provide 'core-completion)
;;; core-completion.el ends here
