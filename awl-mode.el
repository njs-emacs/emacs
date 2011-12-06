(defun awl-mode () (interactive)
  (setq truncate-lines t)
  (setq major-mode 'awl-mode)
  (setq mode-name "AWL")
  (setq grep-spec "*.awl")
  (setq grep-flags "-in")
  (setq rgrep-flags "-in")
  (setq case-fold-search t)
  )

(setq compile-save-modes (adjoin 'awl-mode compile-save-modes))


