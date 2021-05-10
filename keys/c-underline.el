(setq c-underline-map (make-sparse-keymap))
(global-set-key (kbd "C-_") c-underline-map)

(def-key-map "C-_" 'c-underline-map)

(def-key c-underline-map (kbd "E") 'keydef-edit-init)
(def-key c-underline-map (kbd "C-_") 'magit)
