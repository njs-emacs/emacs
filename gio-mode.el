(defun gio-mode () (interactive)
  (setq tab-width 3)
  (setq truncate-lines t)
  (setq major-mode 'gio-mode)
  (setq mode-name "gio")
  (setq grep-spec "*.gio")
  (setq grep-flags "-in")
  (setq rgrep-flags "-in")
  (setq case-fold-search t)
  )

(defun gio-eval-buffer () (interactive)
  (save-buffer)
  (compile (format "perl -Ie:/perl/lib e:/perl/gio.exec.pl %s" (buffer-file-name)))
  )

(setq compile-save-modes (cons 'gio-mode compile-save-modes))
(put 'gio-mode 'eval-buffer-modal 'gio-eval-buffer)

