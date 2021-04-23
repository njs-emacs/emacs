(setq c-j-map (make-sparse-keymap))
(global-set-key (kbd "C-j") c-j-map)

(def-key-map "C-j" 'c-j-map)

(def-key c-j-map (kbd "E") 'keydef-edit-init)

(def-key c-j-map (kbd "C-j") 'symbol-overlay-remove-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://github.com/abo-abo/auto-yasnippet 

(def-key c-j-map (kbd "C-y") 'aya-expand)
(def-key c-j-map (kbd "C-k") 'aya-create)

; aya-persist-snippet

