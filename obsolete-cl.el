(defmacro pop (sym)
  (` (prog1 (car (,sym )) (setq (, sym) (cdr (, sym)))))
  )

(defmacro push (sym x)
  (or (boundp sym) (set sym nil))
  (` (setq (, sym) (cons (, x) (, sym))))
  )

