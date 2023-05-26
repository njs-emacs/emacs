'(setq frame-title-format
   `(multiple-frames "%b" ("" invocation-name "@" system-name))
   )

'(setq frame-title-format
   `(("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position)
   (vc-mode vc-mode)
   "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

;; frame-title-format
;; mode-line-format

;; (rename-frame (selected-frame) "admin - %b")
;; (modify-frame-parameters (selected-frame) (list (cons 'name "admin! - %b")))
;; (modify-frame-parameters (selected-frame) (list (cons 'name nil)))
;; (setq frame-title-format `("" "%b - Admin - " system-name))
