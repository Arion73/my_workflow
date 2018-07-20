;;; core-mode-line.el --- configuration for my emacs

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
(setq evil-insert-state-cursor '("red" bar))
(setq evil-visual-state-cursor '("green" box))
(setq evil-replace-state-cursor '("skyblue" bar))
(setq evil-operator-state-cursor '("gold" hollow))
(setq evil-emacs-state-cursor '("purple" box))

;; evil tag color
  (setq evil-normal-state-tag (propertize " <N> " 'face '((:foreground "DarkOrange" :background "#f9f9f9" :weight bold)))
        evil-insert-state-tag (propertize " <I> " 'face '((:foreground "red" :background "#f9f9f9" :weight bold)))
        evil-visual-state-tag (propertize " <V> " 'face '((:foreground "green" :background "#f9f9f9" :weight bold)))
        evil-replace-state-tag (propertize " <R> " 'face '((:foreground "skyblue" :background "#f9f9f9" :weight bold)))
        evil-operator-state-tag (propertize " <O> " 'face '((:foreground "gold" :background "#f9f9f9" :weight bold)))
        evil-emacs-state-tag (propertize " <E> " 'face '((:foreground "purple" :background "#f9f9f9" :weight bold))))

;; my-mode-line
(defun my-line ()
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
                  ))

    )


;; powerline
(defun power-line ()
    (use-package powerline
      :ensure
      :config
        (custom-set-faces
      '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
      '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
      (powerline-center-evil-theme)))


(provide 'core-mode-line)
