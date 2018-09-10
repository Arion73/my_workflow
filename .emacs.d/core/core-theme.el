;;; core-theme.el

;; Author: Zewei Wang

;; This file is not part of GNU Emacs.

;; Commentary:

;; Configurations for theme.

;; Code:

(setq ns-use-srgb-colorspace nil)

;; monokai-alt-theme
(use-package monokai-alt-theme
  :ensure t)


;; set theme solarized background
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(customize-set-variable 'frame-background-mode 'dark)

(defun set-solarized-light ()
  (interactive)
  (customize-set-variable 'frame-background-mode 'light)
  (load-theme 'solarized t))

(defun set-solarized-dark ()
  (interactive)
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'solarized t))

(set-solarized-dark)

;; gruvbox-theme
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))



(provide 'core-theme)
