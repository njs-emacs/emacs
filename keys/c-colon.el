(setq c-colon-map (make-sparse-keymap))
(global-set-key (kbd "C-:") c-colon-map)

(def-key c-colon-map (kbd "E") 'keydef-edit-init)

(def-key c-colon-map (kbd "C-x") 'er/expand-region)

