(setq c-question-map (make-sparse-keymap))
(global-set-key (kbd "C-?") c-question-map)

(def-key-map "C-?" 'c-question-map)

;(def-key c-question-map (kbd "E") 'keydef-edit-init)

;(def-key c-question-map (kbd "c") 'magit-commit-create)
