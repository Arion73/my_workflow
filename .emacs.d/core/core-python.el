;;; core-python.el --- Configurations for python mode.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for python mode.

;;; Code:


;; elpy
(use-package elpy
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode-hook)
  :config
    ;; highlight-indentation is a dependency package.
    ;; I do not like displaying highlight-indentation as default
    ;(add-hook 'elpy-mode-hook (lambda() (highlight-indentation-mode -1)))
    ;; delete flymake, instead, here I use flycheck
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;; pyenv-mode --- used by elpy
(use-package pyvenv
  :hook (python-mode-hook)
  :config
  (pyvenv-mode t))

;; Python virtualenv mode:
(use-package virtualenvwrapper
  :hook (python-mode-hook)
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs/"))

;; anaconda-mode
(use-package anaconda-mode
  :defer t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; nose --- test runner
(use-package nose
  :hook (python-mode-hook))

;; goto-definition
(defun goto-def-or-rgrep ()
  "Go to definition of thing at point or do an rgrep in project if that fails."
  (interactive)
  (condition-case nil (elpy-goto-definition-other-window)
    (error (elpy-rgrep-symbol (thing-at-point 'symbol)))))

(add-hook 'python-mode-hook
	  (lambda()
	    ;; indent-tabs
	    (setq tab-width 4)
	    (setq python-indent-guess-indent-offset t
		  python-indent-guess-indent-offset-verbose nil)

	    ;; set python-shell-interpreter
	    (setq python-shell-interpreter "python3")
            ;; https://github.com/gregsexton/ob-ipython/issues/89
	    (setq python-shell-prompt-detect-failure-warning nil)
	    ;; https://github.com/gregsexton/ob-ipython/issues/28
	    (setq python-shell-completion-native-enable nil)
	    ;; suppress warnings about python-shell-interpreter doesn't seem to support readline
	    (setq python-shell-completion-native-disabled-interpreters '("python3"))

	    ;; key-bindings
	    (evil-define-key '(normal visual motion) python-mode-map
	      (kbd "SPC c") (lambda()
			       (interactive)
			       (save-buffer)
			       (if (not (string-match-p (regexp-quote "*Python*") (format "%s" (buffer-list))))
				   (run-python))
			       (python-shell-send-buffer t)
			       (if (not (get-buffer-window "*Python*" 'visible))
				   (progn
				     (split-window-below)
				    ;; (if (<= (* 2 (window-height)) (frame-height))
				    ;;	 (enlarge-window 3))
				     (other-window 1)
				     (switch-to-buffer "*Python*"))
				 (other-window 1)))
	      (kbd "SPC C") (lambda()
				(interactive)
				(save-buffer)
				(if (elpy-mode)
				    (elpy-shell-send-region-or-buffer t)
				  (elpy-mode t)
				  (elpy-shell-send-region-or-buffer t))
				(if (not (get-buffer-window "*Python*" 'visible))
				    (progn
				      (split-window-below)
				     ;; (if (<= (* 2 (window-height)) (frame-height))
				     ;;	  (enlarge-window 3))
				      (other-window 1)
				      (switch-to-buffer "*Python*"))
				  (other-window 1))))

	    (evil-define-key '(normal visual motion) python-mode-map
	      (kbd "gd") (lambda ()
			   (interactive)
			   (if (elpy-mode)
			       (goto-def-or-rgrep)
			     (elpy-mode t)
			     (goto-def-or-rgrep))))

	    (which-key-add-key-based-replacements "gd" "goto-definition")
	    (which-key-add-key-based-replacements "SPC c" "python-shell-run")
	    (which-key-add-key-based-replacements "SPC C" "elpy-shell-run"))

	  ;; activate vortualenv when stratup python-mode
	  (venv-workon "python3.7"))


;; py-autopep8
(use-package py-autopep8
  :hook (python-mode-hook)
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


;; add mypy for python type hint checking to flycheck.
;; mypy can do more than flake8
(add-to-list 'load-path (concat my-emacs-directory "private/emacs-flycheck-mypy-master/"))
(require 'flycheck-mypy)
(flycheck-add-next-checker 'python-flake8 'python-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)


;; python-django --- for managing Django projects
(use-package python-django
  :defer t)


;; django-mode
(use-package django-mode
  :defer t
  :config
  (yas/load-directory "~/.emacs.d/elpa/django-mode/snippets")
  (add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode)))



(provide 'core-python)
;;; core-python.el ends here
