;;; core-theme.el --- my own theme customization.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My own theme customization..

;;; Code:

;; load my own theme path
(add-to-list 'custom-theme-load-path (concat my-emacs-directory "private/zelin-theme"))

(setq ns-use-srgb-colorspace nil)

;; load theme for gui and terminal respectivelay
(if (display-graphic-p)
    (progn
      ;; if graphic
      (load-theme 'zelin-dark-02-gui t))
  ;; else
  (load-theme 'zelin-dark-02-terminal t))


;; set background to be transparent
(global-set-key (kbd "C-c C-t") 'loop-alpha)  ;; short-cut key
(setq alpha-list '((85 55) (100 100)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))


(provide 'core-theme)
;;; core-theme.el ends here
