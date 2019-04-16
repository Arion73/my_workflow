;;; core-mode-line.el --- configuration for the mode-line of my emacs.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; After being tired of spaceline frozen, and I have tried other similar mode-line
;; configurations, e.g. smartline, powerline and telephone, I decide to configure
;; my own "ugly-simple-powerful" mode-line as following.

;;; Code:


(require 'evil)
;; set cursor clolor
(setq evil-normal-state-cursor   '("#FFC820" box))
(setq evil-insert-state-cursor   '("#5BE516" bar))
(setq evil-visual-state-cursor   '("#EEEEEE" box))
(setq evil-replace-state-cursor  '("#F11712" bar))
(setq evil-operator-state-cursor '("#1019DE" hollow))
(setq evil-motion-state-cursor   '("#1D9FFF" box))
(setq evil-emacs-state-cursor    '("#B612B8" box))


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



;; window number
(use-package window-numbering
  :config
    (setq window-numbering-assign-func
	  (lambda ()
	    (when (equal (buffer-name) "*Calculator*") 9)))
    (window-numbering-mode))


(defun custom-modeline-window-number ()
  "Display window number."
  (propertize (concat " " (window-numbering-get-number-string) " ")
	      'face 'my-normal-face))


;; set evil tag with character
(setq evil-normal-state-tag   (propertize " <N> " 'face 'my-normal-face)
      evil-insert-state-tag   (propertize " <I> " 'face 'my-insert-face)
      evil-visual-state-tag   (propertize " <V> " 'face 'my-visual-face)
      evil-replace-state-tag  (propertize " <R> " 'face 'my-repace-face)
      evil-operator-state-tag (propertize " <O> " 'face 'my-operator-face)
      evil-motion-state-tag   (propertize " <M> " 'face 'my-motions-face)
      evil-emacs-state-tag    (propertize " <E> " 'face 'my-emacs-face))


;; modified or read-only
(defun custom-modeline-modified ()
  "My mode line modified."
  (cond
   ((buffer-modified-p) (propertize (format " %s " (all-the-icons-faicon "chain-broken" :v-adjust 0.0))
				    'face '(my-normal-face)
				    'help-echo (format "%s" "Buffer is modified.")))
   (buffer-read-only (propertize (format " %s " (all-the-icons-faicon "lock" :v-adjust 0.0))
				 'face '(my-normal-face)
				 'help-echo (format "%s" "Buffer is read-only.")
				 'mouse-face '(:box 1)
				 'local-map (make-mode-line-mouse-map
					     'mouse-1 (lambda () (interactive) (read-only-mode -1)))))
   (t (propertize (format " %s " (all-the-icons-faicon "link" :v-adjust 0.0))
		  'face '(my-normal-face)
		  'help-echo (format "%s" "Buffer is writable and not modified.")
		  'mouse-face '(:box 1)
		  'local-map (make-mode-line-mouse-map
			      'mouse-1 (lambda () (interactive) (read-only-mode 1)))))))


;; directory
(defun custom-modeline-directory ()
  "Display shorten directory of max-length in mode line."
  (let ((max-length 20)
	(path (reverse (split-string (abbreviate-file-name default-directory) "/")))
        (output ""))
       (when (and path (equal "" (car path)))
         (setq path (cdr path)))
       (while (and path (< (length output) (- max-length 4)))
         (setq output (concat (car path) "/" output))
         (setq path (cdr path)))
       (when path
         (setq output (concat "../" output)))
       (if (buffer-file-name)
	   (propertize (concat " " output " ")
		       'face '(:height 0.7)
		       'help-echo (format "%s" buffer-file-name)))))


;; major mode icon
(defun custom-modeline-major-mode ()
  "Display icon for major mode."
  (propertize
   (concat " " (propertize (all-the-icons-icon-for-mode major-mode :height 0.7 :v-adjust 0.1)
			   'help-echo (format "%s" major-mode)) " ")))


;; flycheck status
(defun custom-modeline-flycheck-status-term ()
  "Flycheck status for terminal mode line."
  (require 'flycheck)
  (pcase flycheck-last-status-change
    (`finished (if flycheck-current-errors
 		   (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
			  (no-errors (cdr (assq 'error error-counts)))
			  (no-warnings (cdr (assq 'warning error-counts)))
			  (face (cond (no-errors 'error)
			  	      (no-warnings 'warning)
			  	      (t 'success))))
		     (propertize
		      (concat " "
			      (if (> (or no-errors 0) 0)
				  (propertize (format "•%s" (or no-errors 0)) 'face 'error))
			      " "
			      (if (> (or no-warnings 0) 0)
				  (propertize (format "•%s" (or no-warnings 0)) 'face 'warning))
			      " ")
		       'help-echo (format "Flycheck: %s errors, %s warnings" (or no-errors 0) (or no-warnings 0))
		       'mouse-face '(:box 1)
		       'local-map (make-mode-line-mouse-map
				   'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
		 (propertize (format " ✔ ") 'face 'success)))

    (`not-checked (propertize " ✖ Not Checked " 'face 'font-lock-comment-face))
    (`running (propertize " ⟲ " 'face '(success)))
    (`errored (propertize " ⚠ Error " 'face 'error
			  'help-echo "Flycheck results"
			  'mouse-face '(:box 1)
			  'local-map (make-mode-line-mouse-map
				      'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
    (`no-checker (propertize " ⚠ No Checker " 'face 'warning))
    (`interrupted " Interrupted ")
    (`suspicious (propertize " ?" 'face 'warning))))


;; flycheck icon
(defun custom-modeline-flycheck-status-gui ()
  "Display icons for flycheck in mode line."
  (require 'flycheck)
  (pcase flycheck-last-status-change
    (`finished (if flycheck-current-errors
 		   (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
			  (no-errors (cdr (assq 'error error-counts)))
			  (no-warnings (cdr (assq 'warning error-counts)))
			  (face (cond (no-errors 'error)
			  	      (no-warnings 'warning)
			  	      (t 'success))))
		     (propertize
		      (concat
			      (if (> (or no-errors 0) 0)
				  (propertize (format " %s%s" (all-the-icons-faicon "bug" :v-adjust 0.0) (or no-errors 0))
					      'face 'error))
			      " "
			      (if (> (or no-warnings 0) 0)
				  (concat
				   (propertize (format "%s" (all-the-icons-faicon "bell" :v-adjust 0.1))
					       'face '(warning :height 0.7))
				   (propertize (format "%s " (or no-warnings 0))
					       'face 'warning))))

		       'help-echo (format "Flycheck: %s errors, %s warnings" (or no-errors 0) (or no-warnings 0))
		       'mouse-face '(:box 1)
		       'local-map (make-mode-line-mouse-map
				   'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))

		 (propertize (format " %s " (all-the-icons-faicon "check-circle" :v-adjust 0.0))
			     'face 'success)))

    (`not-checked (propertize (concat " " (all-the-icons-faicon "hourglass-half" :v-adjust 0.1) " ")
			      'face '(:height 0.8)
			      'help-echo "Not Checked"))
    (`running (propertize (concat " " (all-the-icons-faicon "refresh" :v-adjust 0.0) " ")
			  'face 'success))
    (`errored (propertize (format " %s %s " (all-the-icons-faicon "times-circle-o" :v-adjust 0.0) "Error")
			  'face 'error
			  'help-echo "Flycheck results"
			  'mouse-face '(:box 1)
			  'local-map (make-mode-line-mouse-map
				      'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
    (`no-checker (propertize (format " %s " (all-the-icons-faicon "minus-circle" :height 0.8 :v-adjust 0.1)
				     'face 'font-lock-comment-face)
			     'help-echo "No Checker"))
    (`interrupted (propertize (format " %s %s " (all-the-icons-faicon "ban" :v-adjust 0.0)) "Interrupted"))
    (`suspicious (propertize (concat " " (all-the-icons-faicon "question-circle-o" :v-adjust 0.0) " ")
			     'face 'warning))))



;; VC
(defadvice custom-modeline-vc-term (after strip-backend () activate)
  "Replace Git with unicode."
  (when (stringp vc-mode)
    (let ((gitlogo (replace-regexp-in-string "^ Git." " :" vc-mode)))
          (setq vc-mode gitlogo))))


(defun custom-modeline-vc-gui ()
  "VC segment in mode line for GUI."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-octicon "git-branch" :v-adjust 0.0)))
     (propertize (format " %s " branch))
     (propertize " "))))


;; coding system
(defun custom-modeline-coding-system ()
  "Custom coding system for mode line."
  (let* ((code (symbol-name buffer-file-coding-system)))
    (if (and (string-match "utf-8" code) t)
	(setq code "UTF-8"))
    (propertize (concat " " code " ")
		'help-echo (format "%s" buffer-file-coding-system)
		'mouse-face '(:box 1)
		'local-map (make-mode-line-mouse-map
			    'mouse-1 (lambda () (interactive) (describe-coding-system buffer-file-coding-system))))))


;; packages upgrade
(defun custom-modeline-package-updates ()
  "Show number of packages needed to upgrade in mode line."
  (let* ((num (length (package-menu--find-upgrades))))
    (when (> num 0)
      (propertize
       (concat
        (propertize (format " %s" (all-the-icons-faicon "upload" :v-adjust 0.1))
		    'face '(success :height 0.8))
        (propertize (format "%d " num)
		    'face '(success)))
       'help-echo (format "%d Packages need to be upgraded." num)))))


;; left separator
(defun custom-modeline-left-separator ()
  "Left separator."
  (propertize (all-the-icons-alltheicon "cup-left" :v-adjust 0.0)
	      'face '(:foreground "#FFC820" :height 1.2)))

;; right separator
(defun custom-modeline-right-separator ()
  "Right separator."
  (propertize (all-the-icons-alltheicon "cup-right" :v-adjust 0.0)
	      'face '(:foreground "#FFC820" :height 1.2)))


;; function to align left space and right space
(defun mode-line-render (left right)
  "Return a string of 'window-width' length containg Left and Right aligned respectively.  LEFT: left side.  RIGHT: right side."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format "%%s %%%ds" available-width) left right)))


(setq-default mode-line-format
	      '((:eval
		 (mode-line-render
		  ;;left
		  (format-mode-line
		   (list
		    ;; window number
		    (custom-modeline-window-number)

		    ;; evil tag
		    evil-mode-line-tag

		    ;; buffer modified
		    (if (display-graphic-p)
			(list
			 ;; buffer modified
			 (custom-modeline-modified)
			 ;; buffer size
			 (propertize " %I " 'face 'my-normal-face)
			 ;; coding system
			 (custom-modeline-coding-system)
			 ;; line number, column number and percent of buffer above bottom of window
			 (propertize " %02l:%02c | %p%%  " 'face 'my-normal-face)

			 ;; right separator
			 ;(custom-modeline-right-separator)
			 ))

		      ;; client
		      mode-line-client

		      ;; buffer directory
		      (custom-modeline-directory)

		      ;; left separator
		      ;(if (display-graphic-p) (custom-modeline-left-separator))

		      ;; buffer name
		      (propertize " %b " 'face 'my-normal-face)

		      ;; right separator
		      ;(if (display-graphic-p) (custom-modeline-right-separator))

		      ;; major mode icon
		      (if (display-graphic-p)
			  (condition-case nil
			      (custom-modeline-major-mode)
			    (error (propertize " %m")))
			(propertize " %m"))

		      ;; mode-line-process
		      (if mode-line-process (propertize ": %s "))

		      ;; flycheck
		      (if (display-graphic-p)
			  (custom-modeline-flycheck-status-gui)
		       ;(flycheck-mode-line-status-text))
			(custom-modeline-flycheck-status-term))

		      ;; git info
		      (when (stringp vc-mode)
			(if (display-graphic-p)
			    (custom-modeline-vc-gui)
			  (propertize (concat " " vc-mode " "))))

		      ;; package needed to upgrade
		      (custom-modeline-package-updates)

		      ))


		  ;; right
		  (format-mode-line
		   (list
		    mode-line-misc-info

		    ;; virtualenvwrapper
		    (if (stringp venv-current-name)
			(propertize (concat "[" venv-current-name "]")))

		    ;; left separator
		    ;(if (display-graphic-p) (custom-modeline-left-separator))

		    ;; line number, column number and percent of buffer above bottom of window
		    ;;(propertize " %02l:%02c | %p%%  " 'face 'my-normal-face)

		    ;; right separator
		    ;(if (display-graphic-p) (custom-modeline-right-separator))

		    ))))))


(provide 'core-mode-line)
;;; core-mode-line.el ends here
