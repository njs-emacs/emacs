(setq c-plus-map (make-sparse-keymap))
(global-set-key (kbd "C-+") c-plus-map)

(def-key-map "C-+" 'c-plus-map)

(def-key c-plus-map (kbd "E") 'keydef-edit-init)

