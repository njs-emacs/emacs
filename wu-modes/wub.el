(setq wug-map (make-sparse-keymap))
(setq wug-insert-map (make-sparse-keymap))

(defun wug-define (key string)
  (define-key wug-insert-map key string)
  (define-key wug-map key 'wub-replace-with-string)
  )

(defun wug-mode () (interactive) (wub-mode wug-map wug-insert-map))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(wug-define "a" (format "%-10s" "adj"))
(wug-define "n" (format "%-10s" "noun"))
(wug-define "x" (format "%-10s" "expression"))
(wug-define "c" (format "%-10s" "compar"))
(wug-define "vu" (format "%-10s" "verb.u"))
(wug-define "u" (format "%-10s" "verb.u"))
(wug-define "vi" (format "%-10s" "verb.i"))
(wug-define "i" (format "%-10s" "verb.i"))
(wug-define "vp" (format "%-10s" "verb.p"))
(wug-define "p" (format "%-10s" "verb.p"))

(wug-define "1" (format "%-10s" "cardinal"))
(wug-define "r" (format "%-10s" "adv"))
(wug-define "m" (format "%-10s" "misc"))
(wug-define "g" (format "%-10s" "garbage"))
get under way
man
woman
compact discmale female have
