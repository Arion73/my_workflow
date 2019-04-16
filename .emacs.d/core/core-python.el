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
    (set-face-background 'highlight-indentation-face "#333333")
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
				(elpy-shell-send-region-or-buffer t)
				(if (not (get-buffer-window "*Python*" 'visible))
				    (progn
				      (split-window-below)
				     ;; (if (<= (* 2 (window-height)) (frame-height))
				     ;;	  (enlarge-window 3))
				      (other-window 1)
				      (switch-to-buffer "*Python*"))
				  (other-window 1))))

	    (which-key-add-key-based-replacements "SPC c" "python-shell-run")
	    (which-key-add-key-based-replacements "SPC C" "elpy-shell-run"))

	  ;; activate vortualenv when stratup python-mode
	  (venv-workon "python3.7"))


;; py-autopep8
(use-package py-autopep8
  :hook (python-mode-hook)
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


(provide 'core-python)
;;; core-python.el ends here
