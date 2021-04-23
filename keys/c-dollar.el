(setq c-dollar-map (make-sparse-keymap))
(global-set-key (kbd "C-$") c-dollar-map)

(def-key-map "C-$" 'c-dollar-map)

(def-key c-dollar-map (kbd "E") 'keydef-edit-init)

