;;; core-python.el

;; Author: Zewei Wang

;; This file is not part of GNU Emacs.

;; Commentary:

;; Configurations for python mode.

;; Code:

;; Python virtualenv mode:
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location
        (expand-file-name "~/.virtualenvs/")))

;; anaconda-mode
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; nose --- test runner
(use-package nose
  :ensure t)

;; pyenv-mode
(use-package pyvenv :ensure t)

;; indent-tabs
(add-hook 'python-mode-hook
		  (lambda()
			(setq tab-width 4)
			(set-variable 'python-indent-offset 4)
			))



(provide 'core-python)
