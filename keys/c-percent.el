(setq c-percent-map (make-sparse-keymap))
(global-set-key (kbd "C-%") c-percent-map)

(def-key-map "C-%" 'c-percent-map)

(def-key c-percent-map (kbd "E") 'keydef-edit-init)


