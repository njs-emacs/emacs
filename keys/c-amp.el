(setq c-amp-map (make-sparse-keymap))
(global-set-key (kbd "C-&") c-amp-map)

(def-key-map "C-&" 'c-amp-map)

(def-key c-amp-map (kbd "E") 'keydef-edit-init)


