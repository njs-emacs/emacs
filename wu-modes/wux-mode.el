;; this is a mode for fast replacement of numbers in columns

(setq wux-map (make-sparse-keymap))

(defun wux-replace-sexp-with-and-goto-next-line (s) (interactive)
  (sx
   (kill-sexp 1)
   (insert s)
   )
  (next-line 1)
  )

(defun wux-replace-sexp-with-and-goto-next-line (s) (interactive)
  "kill to last blank after first"
  (sx
   (kill-regexp "\\S +\\s +")
   (insert s)
   )
  (next-line 1)
  )

(defun wux-replace-with-number () (interactive)
  (wux-replace-sexp-with-and-goto-next-line
   (format "%2s" (this-command-keys))
   )
  )

(defun wux-mode () (interactive)
  (define-key wux-map [kp-decimal] '(lambda () (interactive) (wux-replace-sexp-with-and-goto-next-line "10")))
  (define-key wux-map [kp-add] 'next-line)

  (define-key wux-map (i 10) (local-set-key (char-to-string (+ ?0 i)) 'wux-replace-with-number))
  (use-local-map wux-map)
)
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq wub-map (make-sparse-keymap))
(setq wub-insert-map (make-sparse-keymap))

(defun wub-replace-with-string () (interactive)
  (wux-replace-sexp-with-and-goto-next-line
   (lookup-key wub-insert-map (this-command-keys))
   )
  )

(defun wub-mode (&optional map insert-map) (interactive)
  (and map (setq wub-map map))
  (and insert-map (setq wub-insert-map insert-map))
  (use-local-map wub-map)
)

(defun wub-define (key string)
  (define-key wub-insert-map key string)
  (define-key wub-map key 'wub-replace-with-string)
  )

(defun wub-unset (key)
  (define-key wub-map key nil)
  (define-key wub-insert-map key nil)
  (setq wub-map (keymap-prune wub-map))
  (setq wub-insert-map (keymap-prune wub-insert-map))
  )

(defun wub-mode-exit () (interactive)
  (use-local-map nil)
)

(wub-unset "a")

(make-variable-buffer-local 'wub-map)
(make-variable-buffer-local 'wub-insert-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq wug-map (make-sparse-keymap))
(setq wug-insert-map (make-sparse-keymap))

(defun wug-define (key string)
  (define-key wug-insert-map key string)
  (define-key wug-map key 'wub-replace-with-string)
  )

(wug-define " " " ")

(defun wug-mode () (interactive) (wub-mode wug-map wug-insert-map))
