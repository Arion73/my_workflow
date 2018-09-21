;;; core-mode-line.el --- configuration for the mode-line of my emacs.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; After being tired of spaceline frozen, and I have tried other similar mode-line
;; configurations, e.g. smartline, powerline and telephone, I decide to configure
;; my own "ugly-simple-powerful" mode-line as following.

;;; Code:

(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    (let ((gitlogo (replace-regexp-in-string "^ Git." " :" vc-mode)))
          (setq vc-mode gitlogo))))


(setq-default ns-use-srgb-colorspace nil)

;; set cursor clolor
(setq evil-normal-state-cursor '("#FFC820" box))
(setq evil-insert-state-cursor '("#5BE516" bar))
(setq evil-visual-state-cursor '("#EEEEEE" box))
(setq evil-replace-state-cursor '("#F11712" bar))
(setq evil-operator-state-cursor '("#1019DE" hollow))
(setq evil-motions-state-cursor '("#1D9FFF" box))
(setq evil-emacs-state-cursor '("#B612B8" box))


;; display window number
(use-package window-numbering
  :ensure t
  :config
    (setq window-numbering-assign-func
	    (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
    (window-numbering-mode))

;; my faces
(defface my-normal-face
  '((t (:foreground "#333333" :background "#FFC820" )))
  "evil-normal color"
  :group 'faces)

(defface my-insert-face
  '((t (:foreground "#333333" :background "#5BE516")))
  "evil-insert color"
  :group 'faces)

(defface my-visual-face
  '((t (:foreground "#333333" :background "#EEEEEE")))
  "evil-visual color"
  :group 'faces)

(defface my-replace-face
  '((t (:foreground "#333333" :background "#F11712")))
  "evil-replace color"
  :group 'faces)

(defface my-operator-face
  '((t (:foreground "#FFFFFF" :background "#1019DE")))
  "evil-operator color"
  :group 'faces)

(defface my-motions-face
  '((t (:foreground "#FFFFFF" :background "#1D9FFF")))
  "evil-operator color"
  :group 'faces)
(defface my-emacs-face
  '((t (:foreground "#FFFFFF" :background "#B612B8" )))
  "evil-emacs color"
  :group 'faces)


;; evil tag color
(setq evil-normal-state-tag
    (propertize " <N> " 'face 'my-normal-face)
    evil-insert-state-tag
    (propertize " <I> " 'face 'my-insert-face)
    evil-visual-state-tag
    (propertize " <V> " 'face 'my-visual-face)
    evil-replace-state-tag
    (propertize " <R> " 'face 'my-repace-face)
    evil-operator-state-tag
    (propertize " <O> " 'face 'my-operator-face)
    evil-motions-state-tag
    (propertize " <M> " 'face 'my-motions-face)
    evil-emacs-state-tag
    (propertize " <E> " 'face 'my-emacs-face))

;; function to align left space and right space
(defun mode-line-render (left right)
  "Return a string of 'window-width' length containg LEFT and Right aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format "%%s %%%ds " available-width) left right)))

(defun my-mode-line ()
    ;; my mode-line-format
    (setq-default mode-line-format
		(list
		'(:eval (mode-line-render
			;;left
			(format-mode-line
			(list 
			    '(:eval (propertize (concat " " (window-numbering-get-number-string) " ") 'face 'my-normal-face))
			    ;; evil tag
			    evil-mode-line-tag
			    " "
			    ;; encoding system
			    mode-line-mule-info
			    ;; buffer modified
			    mode-line-modified
			    ;; buffer read only
			    mode-line-client
			    mode-line-remote
			    ;; buffer size
			    " %I "
			    ;; buffer name
			    '(:eval (propertize " %b " 'face 'my-normal-face))
			    ;; value of "mode-name"
			    '(:eval (propertize " %m "))
			    ;; project name
			    '(:eval (if projectile-mode (when (ignore-errors (projectile-project-root)) (format " Proj[%s] " (projectile-project-name)))))
			    ;; flycheck
			    (flycheck-mode-line-status-text)
			    " "
			    ;; pyvenv-virtual-env-name
			    '(:eval (if pyvenv-mode (when (stringp pyvenv-virtual-env-name)
			   	      (propertize (concat " " pyvenv-virtual-env-name " ") 'face 'my-normal-face))))
			    ;; git info
			    vc-mode
			    ))
			;; right
			(format-mode-line
			(list
			    ;; line number, column number and percent of buffer above bottom of window
			    "  %02l:%02c | %p%%  "
			    mode-line-misc-info
			    )))))))

(my-mode-line)

(provide 'core-mode-line)
;;; core-mode-line.el ends here
