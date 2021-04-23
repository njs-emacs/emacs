(setq bslash-map (make-sparse-keymap))

(global-set-key (kbd "C-\\") bslash-map)

(def-key-map "C-\\" 'bslash-map)

