;;; core-package --- Provide package management.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide package management.

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; count how many packages loaded, and how much time spent
(setq use-package-compute-statistics t)

;; ensure system binaries exist alongside package declations
(use-package use-package-ensure-system-package
  :ensure t
  :defer t)

;; update packages automatically
(use-package auto-package-update
  :ensure t
  :defer t
  :config
  ;; set to update every 30 days
  (setq auto-package-update-interval 30)
  ;; show a prompt before automatic update
  (setq auto-package-update-prompt-before-update t)
  ;; delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; hide results buffer
  ;(setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; test emacs startup time
(use-package esup
  :ensure t
  :defer t)

(provide 'core-package)
;;; core-package.el ends here
