;;; core-evil.el

;; Author: Zewei Wang

;; This file is not part of GNU Emacs.

;; Commentary:

;; Configurations for Evil mode.

;; Code:

;; evil-mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  ;; more configuration goes here
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))


;; evil-matchit --- jump between blocks
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(provide 'core-evil)
