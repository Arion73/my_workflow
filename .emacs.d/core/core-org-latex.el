;;; core-org-latex.el --- Provides functions for better support of exporting org-mode file to latex or pdf file.

;; Author: Z.Wang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configurations for Org-mode.

;;; Code:


;; org2ctex --- for Chinese
(use-package org2ctex
  :hook (org-mode-hook)
  :defer t
  :config
  (org2ctex-toggle t)
  (org2ctex))

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "tex" "org~" "html~" "pdf~")))

	    (evil-define-key '(normal visual motion) org-mode-map
	      (kbd "SPC op") 'org-export-and-move-pdf)
	    ))


(defun org-export-and-move-pdf ()
  "Publish org file to pdf, move the produced pdf file to specific dir and delete original pdf file."
  (interactive)
  (org-latex-export-to-pdf)
  (let ((filename (concat (file-name-sans-extension buffer-file-name) ".pdf"))
	(dest-dir "~/documents/myproject/mynotebook/latex-pdf/"))
    (if (not (and filename (file-exists-p filename)))
	(message "%s do not exists!" filename)
      (shell-command (concat "mv " filename " " dest-dir))
      (delete-file filename)
      (message "%s saved!" (concat dest-dir (file-name-nondirectory filename))))))


(provide 'core-org-latex)
;;; core-org-latex.el ends here
