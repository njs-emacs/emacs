(setq c-lt-map (make-sparse-keymap))
(global-set-key (kbd "C-<") c-lt-map)

(def-key-map "C-<" 'c-lt-map)

;(def-key c-lt-map (kbd "E") 'keydef-edit-init)

(def-key c-lt-map (kbd "l") 'org-agenda-list)
