;;; core-theme.el --- my own theme customization.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My own theme customization..

;;; Code:

;; load my own theme path
(add-to-list 'custom-theme-load-path "~/.emacs.d/private/zelin-theme")

(setq ns-use-srgb-colorspace nil)

;; load theme for gui and terminal respectivelay
(if (display-graphic-p)
    (progn
      ;; if graphic
      (load-theme 'zelin-dark-02-gui t))
  ;; else
  (load-theme 'zelin-dark-02-terminal t))


(provide 'core-theme)
;;; core-theme.el ends here
