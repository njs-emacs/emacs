;; this is a mode for a canvas type buffer perhaps for use with mug or gdb

(defun ecan-define-local-keys ()
 )

(defvar ecan-mode-map nil "")
(setq ecan-mode-map (make-sparse-keymap))

(defun ecan-mode ()
  "Major mode for editing and using ecan buffers."
  (interactive)

  (setq major-mode 'ecan-mode)
  (setq mode-name "ecan")
  (setq ecan-buffer-mru (current-buffer))

  (ecan-define-local-keys)

  (use-local-map ecan-mode-map)
  (emacs-read-hash-plus)
  (emacs-eval-buffer-delimited-region "^#={" "^#=}")
  )

(setq auto-mode-alist (alist-put auto-mode-alist "\\.ecan$" 'ecan-mode))

(provide 'ecan)

;;; ecan.el ends here
