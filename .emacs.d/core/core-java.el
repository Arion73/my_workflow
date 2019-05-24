;;; core-java.el --- Configurations for java mode.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for java mode.

;;; Code:



;; my run java function and keybindings
(defun my-run-java ()
  "My run go program function and keybindings."
  (interactive)
  (save-buffer)
  (let* ((default-directory (file-name-directory (buffer-file-name)))
	(file-name (file-name-nondirectory buffer-file-name))
	(base-title "java")
	(term-name "*java*"))
    (if (not (string-match-p (regexp-quote term-name) (format "%s" (buffer-list))))
	(progn
	  (set-buffer (make-term base-title explicit-shell-file-name))
	  (term-mode)
	  (term-char-mode)))
    (comint-send-string term-name "clear\n")
    (comint-send-string term-name (concat "java " file-name "\n"))
    (if (not (get-buffer-window term-name 'visible))
	(progn
	  (split-window-below)
	 ;; (if (<= (* 2 (window-height)) (frame-height))
	 ;;     (enlarge-window 3))
	  (other-window 1)
	  (switch-to-buffer term-name))
      (other-window 1))))

(add-hook 'java-mode-hook
          (lambda ()
            (evil-define-key '(normal visual motion) java-mode-hook
              (kbd "SPC c") 'my-run-java)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Meghanada-mode --- A better java development environment
(use-package autodisass-java-bytecode
  :defer t)

(use-package google-c-style
  :defer t
  :commands
  (google-set-c-style))

(use-package meghanada
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)
              (meghanada-mode t)
              (smartparens-mode t)
              (rainbow-delimiters-mode t)
              (highlight-symbol-mode t)
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

  :config
  (use-package realgud)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  :bind
  (:map meghanada-mode-map
        ("C-S-t" . meghanada-switch-testcase)
        ("M-RET" . meghanada-local-variable)
        ("C-M-." . helm-imenu)
        ("M-r" . meghanada-reference)
        ("M-t" . meghanada-typeinfo)
        ("C-z" . hydra-meghanada/body))
  :commands
  (meghanada-mode))

(defhydra hydra-meghanada (:hint nil :exit t)
"
^Edit^                           ^Tast or Task^
^^^^^^-------------------------------------------------------
_f_: meghanada-compile-file      _m_: meghanada-restart
_c_: meghanada-compile-project   _t_: meghanada-run-task
_o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
_s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
_v_: meghanada-local-variable    _R_: meghanada-run-junit-recent
_i_: meghanada-import-all        _r_: meghanada-reference
_g_: magit-status                _T_: meghanada-typeinfo
_l_: helm-ls-git-ls
_q_: exit
"
  ("f" meghanada-compile-file)
  ("m" meghanada-restart)

  ("c" meghanada-compile-project)
  ("o" meghanada-optimize-import)
  ("s" meghanada-switch-test-case)
  ("v" meghanada-local-variable)
  ("i" meghanada-import-all)

  ("g" magit-status)
  ("l" helm-ls-git-ls)

  ("t" meghanada-run-task)
  ("T" meghanada-typeinfo)
  ("j" meghanada-run-junit-test-case)
  ("J" meghanada-run-junit-class)
  ("R" meghanada-run-junit-recent)
  ("r" meghanada-reference)

  ("q" exit)
  ("z" nil "leave"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Another choice for java env --- lsp
;;; Language Server Protocol
;;  ------------------------
(use-package lsp-mode
  :defer t
  :init (setq lsp-inhibit-message t
              lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil))

(use-package company-lsp
  :after  lsp-mode
  :config
  (add-hook 'java-mode-hook
            (lambda () (push 'company-lsp company-backends)))
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))


(use-package lsp-ui
  :after lsp-mode
  :preface
  (defun lsp-ui-display-doc ()
    "A command to issue document view requests."
    (interactive)
    (require 'markdown-mode)
    (lsp--send-request-async
     (lsp--make-request "textDocument/hover" (lsp--text-document-position-params))
     (lambda (hover)
       (popup-tip (replace-regexp-in-string markdown-regex-link-inline
                                            "\\3"
                                            (lsp-ui-doc--extract (gethash "contents" hover))
                                            t
                                            nil)
                  :nostrip t
                  :height 100))))
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-update-mode 'point))

(use-package lsp-java
  :after lsp-mode
  :commands lsp-java-enable
  :init
  (add-hook 'java-mode-hook (lambda ()
                              (flycheck-mode +1)
                              (lsp-java-enable)
                              (lsp-ui-flycheck-enable t)
                              (lsp-ui-sideline-mode)))
  :config
  (setq lsp-java-server-install-dir
        (expand-file-name "~/.local/eclips.jdt.ls/server/")
        lsp-java-workspace-dir
        (expand-file-name "~/code/eclipse.jdt.ls/"))

  ;; Do not organize imports on file save
  (setq lsp-java-save-action-organize-imports nil)

  (when (not (file-exists-p lsp-java-server-install-dir))
    ;; Install Eclipse JDT server
    (message "Installing JDT server for emacs!")
    (async-shell-command
     (format (concat "mkdir -p %s;"
                     "wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz -O /tmp/jdt-latest.tar;"
                     "tar xf /tmp/jdt-latest.tar -C %s;")
             lsp-java-server-install-dir
             lsp-java-server-install-dir))))




(provide 'core-java)
;;; core-java.el ends here
