;;; core-c-c++.el --- configuration for the C/C++ layer of my emacs.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide C/C++ configurations.

;;; Code:


;; diaster
(add-to-list 'load-path (concat my-emacs-directory "private/disaster-master/"))
(require 'disaster)

;; functions of runing c/c++ program and  key bindings
(defvar cpp-generate-compiler "g++ -g -O2 -std=c++0x")

(defun cpp-generate-makefile ()
  (interactive)
  (let* ((buffer-name (buffer-file-name))
         (file-name (file-name-nondirectory buffer-name))
         (target-name (file-name-sans-extension file-name))
         (make-file (concat (file-name-directory buffer-name) "Makefile")))
    (if (file-exists-p make-file)
        (when (called-interactively-p 'any)
          (message "Makefile already exists"))
      (with-current-buffer (find-file-noselect make-file)
        (insert
         (concat target-name ": " file-name
                 (format "\n\t%s -o $@ $^" cpp-generate-compiler)
                 "\n\nclean: \n\trm -f " target-name
                 "\n\nrun: " target-name "\n\t ./" target-name
                 "\n\n.PHONY: clean run\n"))
        (save-buffer)))))

(defun cpp-run ()
  (interactive)
  (cpp-generate-makefile)
  (compile "make run" t))

(defun c-run ()
  (interactive)
  (let ((cpp-generate-compiler "gcc -g -O2 -std=c99"))
    (cpp-run)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (evil-define-key '(normal visual motion) c-mode-map
	      (kbd "SPC C") 'c-run)))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (evil-define-key '(normal visual motion) c++-mode-map
	      (kbd "SPC C") 'cpp-run)))

(setq compilation-ask-about-save nil)

(setq compilation-finish-functions
      (list (lambda(buffer str)
              (unless (string= str "finished\n")
                (push-mark)
                (next-error)))))


;; my run C function and keybindings
(defun my-run-c ()
  "My run C program function and keybindings."
  (interactive)
  (save-buffer)
  (let* ((default-directory (file-name-directory (buffer-file-name)))
	(file-name (file-name-nondirectory buffer-file-name))
	(execution-name (file-name-sans-extension file-name))
	(base-title "Terminal-C")
	(term-name "*Terminal-C*"))
    (if (not (string-match-p (regexp-quote term-name) (format "%s" (buffer-list))))
	(progn
	  (set-buffer (make-term base-title explicit-shell-file-name))
	  (term-mode)
	  (term-char-mode)))
    (comint-send-string term-name "clear\n")
    (comint-send-string term-name (concat "gcc -g -o " execution-name " " file-name "\n"))
    (comint-send-string term-name (concat "./" execution-name "\n"))
    (if (not (get-buffer-window term-name 'visible))
	(progn
	  (split-window-below)
	 ;; (if (<= (* 2 (window-height)) (frame-height))
	 ;;     (enlarge-window 3))
	  (other-window 1)
	  (switch-to-buffer term-name))
      (other-window 1))))

(add-hook 'c-mode-hook
	  (lambda ()
	    (evil-define-key '(normal visual motion) c-mode-map
	      (kbd "SPC c") 'my-run-c)))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (evil-define-key '(normal visual motion) c++-mode-map
	      (kbd "SPC c") 'my-run-c)))


(provide 'core-c-c++)
;;; core-c-c++.el ends here
