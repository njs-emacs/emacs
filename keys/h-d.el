(setq h-d-map (make-sparse-keymap))

(global-set-key (kbd "H-d") h-d-map)

(def-key-map "H-d" 'h-d-map)

(def-key h-d-map (kbd "H-x") 'boo-boo)
(def-key h-d-map (kbd "H-d") 'boo-boo-again)

(def-key h-d-map (kbd "H-m") 'boo-boom)

(def-key h-d-map (kbd "H-s") 'boo-bosr)
(def-key h-d-map (kbd "H-f") 'boo-bofr)
(def-key h-d-map (kbd "H-a") 'boo-bosa)
(def-key h-d-map (kbd "H-g") 'boo-bofa)

; prefix is used for boo.pl functions

