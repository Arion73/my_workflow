;;; core-mode-line.el --- configuration for the mode-line of my emacs.

;; Author: Zewei Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; After being tired of spaceline frozen, and I have tried other similar mode-line
;; configurations, e.g. smartline, powerline and telephone, I decide to configure
;; my own "ugly-simple-powerful" mode-line as following.

;;; Code:

(setq-default ns-use-srgb-colorspace nil)

;; set cursor clolor
(setq evil-normal-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-visual-state-cursor '("gray" box))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("gold" hollow))
(setq evil-emacs-state-cursor '("skyblue" box))

;; my-mode-line
(defun my-line ()
  ;; evil tag color
  (setq evil-normal-state-tag
        (propertize " <N> " 'face '((:foreground "DarkOrange" :background "#f9f9f9" :weight bold)))
        evil-insert-state-tag
        (propertize " <I> " 'face '((:foreground "red" :background "#f9f9f9" :weight bold)))
        evil-visual-state-tag
        (propertize " <V> " 'face '((:foreground "green" :background "#f9f9f9" :weight bold)))
        evil-replace-state-tag
        (propertize " <R> " 'face '((:foreground "skyblue" :background "#f9f9f9" :weight bold)))
        evil-operator-state-tag
        (propertize " <O> " 'face '((:foreground "gold" :background "#f9f9f9" :weight bold)))
        evil-emacs-state-tag
        (propertize " <E> " 'face '((:foreground "purple" :background "#f9f9f9" :weight bold))))

  ;; mode-line-format
  (setq-default mode-line-format
                  (list
                  ;; evil tag
                  '(:eval evil-mode-line-tag)
                  ;; separator

                  ;; buffer encoding
                  mode-line-mule-info
                  ;; buffer state
                  " %*"
                  ;; buffer size
                  " %I"
                  " "
                  ;; buffer name
                  '(:eval (propertize "%b" 'face '((:weight bold))))
                  " "
                  ;; major mode
                  '(:eval (propertize "%m"))
                  "   "
                  ;; line number & column number
                  "(%02l,%02c)"
                  " | "
                  ;; percent of buffer above bottom of window
                  "%p"
                  " "
                  ;; git info
                  '(vc-mode vc-mode)
                  " "
                  )))

(defun space-line ()
  ;; display battery info
  (use-package fancy-battery
    :ensure t
    :config
    (add-hook 'after-init-hook #'fancy-battery-mode))
  ;; display window number
  (use-package window-numbering
    :ensure t
    :config
    (setq window-numbering-assign-func
	        (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
    (window-numbering-mode))
  ;; the spaceline package
  (use-package spaceline
    :ensure t
    :config
    (require 'spaceline-config)
    (setq powerline-default-separator 'wave)
    (spaceline-toggle-window-number-on)
    (spaceline-toggle-minor-modes-off)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (spaceline-spacemacs-theme))

  )


;; set mode-line
;;(my-line)
(space-line)


(provide 'core-mode-line)
