(setq h-slash-map (make-sparse-keymap))

(global-set-key (kbd "H-/") h-slash-map)

(def-key h-slash-map (kbd "E") 'keydef-edit-init)

;(def-key global-map (kbd "H-/ H-/") "->")
