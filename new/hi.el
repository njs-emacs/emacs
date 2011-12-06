(load "hilit19")

(defmacro foo (a b c)
  (hilit-highlight-region (point-min) (point-max)
			  (list nil (list (eval a) (eval b)
					  (hilit-lookup-face-create c))))
  nil
  )

(ow (foo "set-face" 0 ForestGreen))
(ow (foo "ext" 0 salmon))
(ow (foo "ext" 0 red-bold-italic))
(ow (foo "void" ")" white/red))
(ow (foo "extern" 0 red/white))
(ow (foo "static" 0 salmon/yellow))


(hilit-lookup-face-create 'red-bold-italic)
(hilit-lookup-face-create 'pink/black)

(setq hilit-auto-rehighlight nil)
(setq hilit-auto-highlight nil)
