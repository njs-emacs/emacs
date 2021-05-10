(setq s-b-map (make-sparse-keymap))

(global-set-key (kbd "s-b") s-b-map)

(def-key-map "s-b" 's-b-map)

(def-key s-b-map (kbd "s-x") 'boo-boo)
(def-key s-b-map (kbd "s-b") 'boo-boo-again)

(def-key s-b-map (kbd "s-m") 'boo-boom)

(def-key s-b-map (kbd "p") 'boo-bosa)
(def-key s-b-map (kbd "C-p") 'boo-bosr)
(def-key s-b-map (kbd "o") 'boo-bofa)
(def-key s-b-map (kbd "C-o") 'boo-bofr)

(def-key s-b-map (kbd "s") 'boo-bosa)
(def-key s-b-map (kbd "C-s") 'boo-bosr)
(def-key s-b-map (kbd "f") 'boo-bofa)
(def-key s-b-map (kbd "C-f") 'boo-bofr)

; s-b prefix is used for boo.pl functions

