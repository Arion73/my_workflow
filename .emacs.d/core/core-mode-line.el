;;; core-mode-line.el --- configuration for the mode-line of my emacs.

;; Author: Zewei Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; After being tired of spaceline frozen, and I have tried other similar mode-line
;; configurations, e.g. smartline, powerline and telephone, I decide to configure
;; my own "ugly-simple-powerful" mode-line as following.

;;; Code:

(setq-default ns-use-srgb-colorspace nil)

;; set mode-line background color
;;(set-face-background 'mode-line "black")
;;(set-face-background 'mode-line-inactive "shadow")
;;(set-face-attribute 'mode-line nil :height 100)

;; set cursor clolor
(setq evil-normal-state-cursor '("systemYellowColor" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-visual-state-cursor '("gray" box))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("gold" hollow))
(setq evil-emacs-state-cursor '("skyblue" box))


;; display window number
(use-package window-numbering
  :ensure t
  :config
    (setq window-numbering-assign-func
	    (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
    (window-numbering-mode))

;; my faces
(defface my-normal-face
  '((t (:foreground "systemYellowColor" :background "black")))
  "evil-normal color"
  :group 'faces)

(defface my-insert-face
  '((t (:foreground "green" :background "black")))
  "evil-insert color"
  :group 'faces)

(defface my-visual-face
  '((t (:foreground "gray" :background "black")))
  "evil-visual color"
  :group 'faces)

(defface my-repace-face
  '((t (:foreground "skyblue" :background "black")))
  "evil-replace color"
  :group 'faces)

(defface my-operator-face
  '((t (:foreground "gold" :background "black")))
  "evil-operator color"
  :group 'faces)

(defface my-motions-face
  '((t (:foreground "purple" :background "black")))
  "evil-operator color"
  :group 'faces)
(defface my-emacs-face
  '((t (:foreground "purple" :background "black")))
  "evil-emacs color"
  :group 'faces)

(defface my-highlight-face
  '((t (:background "systemYellowColor")))
  "my-red-face"
  :group 'face)

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
			'(:eval (propertize (concat " " (window-numbering-get-number-string) " |") 'face 'my-normal-face))
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
			;; flycheck
			(flycheck-mode-line-status-text)
			" "
			;; git info
		        vc-mode
			" "
			;; pyvenv-virtual-env-name
			'(:eval (when (> (length pyvenv-virtual-env-name) 3)
				  (propertize (concat " " pyvenv-virtual-env-name " ") 'face 'my-normal-face)))
			 ))
		      ;; right
		      (format-mode-line
		       (list
			mode-line-front-space
			;;mode-line-misc-info
			 ;; line number, column number and percent of buffer above bottom of window
			" %02l:%02c | %p%%"
			 )))))))

;(my-mode-line)

(provide 'core-mode-line)


