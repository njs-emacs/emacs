(setq c-colon-map (make-sparse-keymap))
(global-set-key (kbd "C-:") c-colon-map)

(def-key-map "C-:" 'c-colon-map)

;(def-key c-colon-map (kbd "E") 'keydef-edit-init)

(def-key c-colon-map (kps "88") 'minibuffer-dir-show) ; kp-488


