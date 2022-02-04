(setq c-star-map (make-sparse-keymap))
(global-set-key (kbd "C-*") c-star-map)

(def-key-map "C-*" 'c-star-map)

;(def-key c-star-map (kbd "E") 'keydef-edit-init)

