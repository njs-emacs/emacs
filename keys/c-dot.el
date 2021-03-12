(setq c-dot-map (make-sparse-keymap))
(global-set-key (kbd "C-.") c-dot-map)

(def-key c-dot-map (kbd "E") 'keydef-edit-init)

(def-key c-dot-map (kbd "C-.") 'symbol-overlay-put)
