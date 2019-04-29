(autoload 'todo-mode "todo")

(defmacro mapautoload (file &rest syms)
  (progn (mapcar '(lambda (x) (autoload x file nil t)) syms) nil)
)

(autoload 'time-string "time-string.el" "" t)

(autoload 'query "query")

(mapautoload "replace.el"
  buffer-replace
  buffer-replace-lines
  )

(mapautoload "shell.el"
  call-shell
  shell-output
  )

(mapautoload "marker.el"
  define-key-marker
  )

(mapautoload "c-gen.el"
  c-func
  c-in
  c-block
  )

(mapautoload "flash.el"
  flash
  flash-once
  )

(mapautoload "random"
  random-mod
  random-rng
  random-p
  random-nth)

(load "autoload/dos.el" t t)

(load-overrides "autoload")

;(define-key global-map "\M-g" 'gud-quick)
;(autoload 'gud-quick "gud")

(autoload 'makefile-mode "make-mod")

;; needs rewrite for compatability with real sql modes

; (autoload 'sqlint "sql")
