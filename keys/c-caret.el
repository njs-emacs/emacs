(setq c-caret-map (make-sparse-keymap))
(global-set-key (kbd "C-^") c-caret-map)

(def-key c-caret-map (kbd "E") 'keydef-edit-init)

