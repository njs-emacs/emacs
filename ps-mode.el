(setq ps-mode-map (make-sparse-keymap))

(put 'ps-mode 'eval-buffer-modal 'ps-eval-buffer)

(defun ps-mode () (interactive)
  (setq mode-name "ps")
  (setq major-mode 'ps-mode)
  (use-local-map ps-mode-map)
  )

(load "ps-acrobat.el")
;(load "ps-gs.el")

