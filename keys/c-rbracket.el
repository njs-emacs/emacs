(setq c-rbracket-map (make-sparse-keymap))
(global-set-key (kbd "C-]") c-rbracket-map)

(def-key-map "C-]" 'c-rbracket-map)

(def-key c-rbracket-map (kbd "E") 'keydef-edit-init)

(def-key c-rbracket-map (kbd "5") 'indent-pattern-to-column)
(def-key c-rbracket-map (kbd "f") 'find-file)
(def-key c-rbracket-map (kbd "e") 'list-buffers)
(def-key c-rbracket-map (kbd "d") 'dired-quick-dot)

(def-key c-rbracket-map (kbd "a") "aaa")

(def-key c-rbracket-map (kbd "w w") 'ace-window)

