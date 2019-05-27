;;; core-go.el --- Configurations for go mode.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for go mode.

;;; Code:


;; go-mode --- go major mode
(use-package go-mode
  :defer t)


;; go-eldoc --- shows type information for variable, functions, and current
;;              argument position of function.
(use-package go-eldoc
  :hook (go-mode-hook)
  :config
  (go-eldoc-setup))


;; flycheck-golangci-lint --- Flycheck checker for golangci-lint
(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-enable-all t))


(add-hook 'after-init-hook
	  (lambda ()
	    ;; go-errcheck --- invoke errcheck from within Emacs
	    (add-to-list 'load-path (concat my-emacs-directory "private/go-errcheck.el-master/"))
	    (require 'go-errcheck)

	    ;; company-go --- completion used with company
	    (add-to-list 'load-path (concat my-emacs-directory "private/"))
	    (require 'company-go)
	    (add-hook 'go-mode-hook
		      (lambda ()
			(set (make-local-variable 'company-backends) '(company-go))
			(company-mode)))
	    ))


(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
			  ;; gofmt
			  ;(setq gofmt-command "goimports")

			  (setq tab-width 2 indent-tabs-mode 1)

			  ;; format current buffer when save
			  (add-hook 'before-save-hook 'gofmt-before-save)

			  ;; company-go
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)

			  ;; remove all unused imports
			  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
			  ;; go to imports
			  (local-set-key (kbd "C-c C-g") 'go-goto-imports)
			  ;; format current buffer
			  (local-set-key (kbd "C-c C-f") 'gofmt)
			  ;; show the go document for a given package
			  (local-set-key (kbd "C-c C-k") 'godoc)
			  ;; Godef jump key binding
			  (local-set-key (kbd "C-c C-j") 'godef-jump)
			  (local-set-key (kbd "C-c C-p") 'pop-tag-mark)

			  ;; compile go program
			  (setq compile-command "go build -v && go test -v && go vet")
			  (local-set-key (kbd "C-c C-c") 'compile)

			  (local-set-key (kbd "C-c C-b") 'my-run-go)
			  ))


;; my run go function and keybindings
(defun my-run-go ()
  "My run go program function and keybindings."
  (interactive)
  (save-buffer)
  (let* ((default-directory (file-name-directory (buffer-file-name)))
	(file-name (file-name-nondirectory buffer-file-name))
	(base-title "Terminal-go")
	(term-name "*Terminal-go*"))
    (if (not (string-match-p (regexp-quote term-name) (format "%s" (buffer-list))))
	(progn
	  (set-buffer (make-term base-title explicit-shell-file-name))
	  (term-mode)
	  (term-char-mode)))
    (comint-send-string term-name "clear\n")
    (comint-send-string term-name (concat "go run " file-name "\n"))
    (if (not (get-buffer-window term-name 'visible))
	(progn
	  (split-window-below)
	 ;; (if (<= (* 2 (window-height)) (frame-height))
	 ;;     (enlarge-window 3))
	  (other-window 1)
	  (switch-to-buffer term-name))
      (other-window 1))))

(add-hook 'go-mode-hook (lambda ()
			  (evil-define-key '(normal visual motion) go-mode-hook
			    (kbd "SPC c") 'my-run-go) ;; TODO keybinding does not work
			  ))


;; goto definition
(defvar golang-goto-stack '())

(defun golang-jump-to-definition ()
  (interactive)
  (add-to-list 'golang-goto-stack
               (list (buffer-name) (point)))
  (godef-jump (point) nil))

(defun golang-jump-back ()
  (interactive)
  (let ((p (pop golang-goto-stack)))
    (if p (progn
            (switch-to-buffer (nth 0 p))
            (goto-char (nth 1 p))))))



(provide 'core-go)
;;; core-go.el ends here
