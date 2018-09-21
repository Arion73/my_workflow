;;; core-python.el --- Configurations for python mode.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for python mode.

;;; Code:


;; elpy
(use-package elpy
  :ensure t
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode-hook)
  :config
    ;; highlight-indentation is a dependency package.
    ;; I do not like displaying highlight-indentation as default
    (add-hook 'elpy-mode-hook (lambda() (highlight-indentation-mode -1)))
    ;; delete flymake, instead, here I use flycheck
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (elpy-mode t)
    (elpy-enable)
    )

;; Python virtualenv mode:
(use-package virtualenvwrapper
  :ensure t
  :hook (python-mode-hook)
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location
        (expand-file-name "~/.virtualenvs/")))

;; anaconda-mode
(use-package anaconda-mode
  :ensure t
  :defer t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; nose --- test runner
(use-package nose
  :ensure t
  :hook (python-mode-hook))

;; pyenv-mode
(use-package pyvenv
  :ensure t
  :hook (python-mode-hook)
  :config
  (pyvenv-mode t))

;; indent-tabs
(add-hook 'python-mode-hook
	  (lambda()
	    (setq tab-width 4)
	    (setq python-indent-guess-indent-offset t
		  python-indent-guess-indent-offset-verbose nil)

	    ;; set python-shell-interpreter
	    (setq python-shell-interpreter "python3")

	    ;; key-bindings
	    (evil-define-key '(normal visual motion) python-mode-map
	      (kbd "SPC py") 'run-python
	      (kbd "SPC cc") (lambda() (interactive) (python-shell-send-buffer t))
	      (kbd "C-c C-c") (lambda() (interactive) (elpy-shell-send-region-or-buffer t))
	      )))

;; suppress warnings about python-shell-interpreter doesn't seem to support readline
(setq python-shell-completion-native-disabled-interpreters '("python3"))

;; py-autopep8
(use-package py-autopep8
  :ensure t
  :hook (python-mode-hook)
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


(provide 'core-python)
;;; core-python.el ends here
