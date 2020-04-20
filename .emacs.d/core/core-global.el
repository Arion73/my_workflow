;;; core-global.el --- Provide global packages configurations for all buffers.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some packages would be used in all buffers or several modes.
;; Here collects all these packages configurations.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rainbow-delimiters
(use-package rainbow-delimiters
  :diminish ""
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "gray"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "brown"))))
   '(rainbow-delimiters-unmatched-face ((t (:background "maroon")))))
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :strike-through t)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (rainbow-delimiters-mode t))


;; highlight-parentheses
(use-package highlight-parentheses
  :defer t
  :diminish ""
  :config
  (global-highlight-parentheses-mode))


;; view large file
(use-package vlf :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; flycheck --- syntax checking
(use-package flycheck
  :defer t
  :config
  ;; fix problem of go vet error
  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet))))

  (global-flycheck-mode t))

(evil-define-key '(normal visual motion) global-map
  (kbd "SPC fl") 'flycheck-list-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit
(use-package magit
  :defer t
  :config
  (setq magit-refresh-status-buffer nil))

;; highlight version changes on the fringe.
(use-package diff-hl
  :config
  (global-diff-hl-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; docker --- Supports docker containers, images, volumes, networks,
;; docker-machine and docker-compose.
(use-package docker
  :defer t
  :bind ("C-c d" . docker)
  ;:custom (docker-image-run-arguments '("-i" "-t" "--rm"))
  )

;; dockerfile --- handling dockerfiles
(use-package dockerfile-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pandoc-mode
(use-package pandoc-mode
  :defer t
  :hook (markdown-mode-hook org-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; realgud --- debugger
(use-package realgud
  :defer t
  :config
  ;; for python pdb
  (setq realgud:pdb-command-name "python -m pdb")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; openwith --- open files with external applications
(use-package openwith
  :defer t
  :config
  (when (require 'openwith nil 'noerror)
    (setq openwith-associations
	  (list
	   (list (openwith-make-extension-regexp
		  '("mpg" "mpeg"  "mp4"
		    "avi" "wmv" "wav" "mov" "flv"
		    "ogm" "ogg" "mkv"))
		 "mpv"
		 '(file))
	   ))
    (openwith-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-global)
;;; core-global.el ends here
