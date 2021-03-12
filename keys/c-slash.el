(setq c-slash-map (make-sparse-keymap))
(global-set-key (kbd "C-/") c-slash-map)

(def-key c-slash-map (kbd "E") 'keydef-edit-init)

(def-key c-slash-map (kbd "C-/") 'symbol-overlay-rename)
(def-key c-slash-map (kbd "C-f") 'org-insert-file-link)
