(setq c-tilde-map (make-sparse-keymap))
(global-set-key (kbd "C-~") c-tilde-map)

(def-key-map "C-~" 'c-tilde-map)

(def-key c-tilde-map (kbd "E") 'keydef-edit-init)

(def-key c-tilde-map (kbd "C-x") 'er/expand-region)

