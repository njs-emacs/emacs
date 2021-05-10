(setq s-s-map (make-sparse-keymap))

(global-set-key (kbd "s-s") s-s-map)

(def-key-map "s-s" 's-s-map)

(def-key s-s-map (kbd "d") 'yas-describe-tables)

; s-s prefix is used for snippet functions
; snippet mapping does not use normal define-key operations
; use binding: 
