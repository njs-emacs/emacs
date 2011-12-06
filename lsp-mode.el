(defun lsp-mode () (interactive)
  (lisp-mode)
  (setq tab-width 3)
  (setq truncate-lines t)
  (setq major-mode 'lsp-mode)
  (setq mode-name "lsp")
  (setq grep-spec "*.lsp")
  (setq grep-flags "-in")
  (setq rgrep-flags "-in")
  (setq case-fold-search t)
  )

(defun lsp-eval-buffer () (interactive)
  (save-buffer)
  (compile (format "perl -Ie:/perl/lib e:/perl/lsp.exec.pl %s" (buffer-file-name)))
  )

(setq compile-save-modes (cons 'lsp-mode compile-save-modes))
(put 'lsp-mode 'eval-buffer-modal 'lsp-eval-buffer)

(add-mode "\\.lsp$" 'lsp-mode)
(autoload 'lsp-mode "lsp-mode")

