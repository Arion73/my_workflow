;;; core-c-c++.el --- configuration for the C/C++ layer of my emacs.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide C/C++ configurations.

;;; Code:


;; diaster
(add-to-list 'load-path "~/.emacs.d/private/disaster-master")
(require 'disaster)

;; functions of runing c/c++ program and  key bindings
(defvar cpp-generate-compiler "g++ -g -O2 -std=c++0x")

(defun cpp-generate-makefile ()
  (interactive)
  (let* ((n-buffer (buffer-file-name))
         (n-file (file-name-nondirectory n-buffer))
         (n-target (file-name-sans-extension n-file))
         (n-makefile (concat
                       (file-name-directory n-buffer)
                       "Makefile")))
    (if (file-exists-p n-makefile)
        (when (called-interactively-p 'any)
          (message "Makefile already exists"))
      (with-current-buffer (find-file-noselect n-makefile)
        (insert
         (concat n-target ": " n-file
                 (format "\n\t%s -o $@ $^"
                   cpp-generate-compiler)
                 "\n\nclean: \n\trm -f " n-target
                 "\n\nrun: " n-target "\n\t ./" n-target
                 "\n\n.PHONY: clean run\n"))
        (save-buffer)))))

(defun cpp-run ()
  (interactive)
  (save-buffer)
  (cpp-generate-makefile)
  (compile "make run"))

(defun c-run ()
  (interactive)
  (let ((cpp-generate-compiler "gcc -g -O2 -std=c99"))
    (cpp-run)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (evil-define-key '(normal visual motion) c-mode-map
	      (kbd "SPC cc") 'c-run)
	    (which-key-add-key-based-replacements "SPC c" "run")))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (evil-define-key '(normal visual motion) c++-mode-map
	      (kbd "SPC cc") 'cpp-run)
	    (which-key-add-key-based-replacements "SPC c" "run")))

(setq compilation-ask-about-save nil)

(setq compilation-finish-functions
      (list (lambda(buffer str)
              (unless (string= str "finished\n")
                (push-mark)
                (next-error)))))


(provide 'core-c-c++)
;;; core-c-c++.el ends here
