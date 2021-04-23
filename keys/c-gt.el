(setq c-gt-map (make-sparse-keymap))
(global-set-key (kbd "C->") c-gt-map)

(def-key-map "C->" 'c-gt-map)

(def-key c-gt-map (kbd "E") 'keydef-edit-init)

(def-key c-gt-map (kbd "c") 'magit-commit-create)


