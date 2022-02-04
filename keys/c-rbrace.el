(setq c-rbrace-map (make-sparse-keymap))
(global-set-key (kbd "C-}") c-rbrace-map)

(def-key-map "C-}" 'c-rbrace-map)

;(def-key c-rbrace-map (kbd "E") 'keydef-edit-init)

;(def-key c-rbrace-map (kbd "c") 'magit-commit-create)

