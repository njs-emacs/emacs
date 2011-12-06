(setq todo-open-0-face (make-face 'todo-open-0-face))
(set-face-foreground 'todo-open-0-face "white")
(set-face-background 'todo-open-0-face "red")

(setq todo-open-1-face (make-face 'todo-open-1-face))
(set-face-foreground 'todo-open-1-face "white")
(set-face-background 'todo-open-1-face "orange")

(setq todo-open-2-face (make-face 'todo-open-2-face))
(set-face-foreground 'todo-open-2-face "black")
(set-face-background 'todo-open-2-face "yellow")

(setq todo-open-3-face (make-face 'todo-open-3-face))
(set-face-foreground 'todo-open-3-face "black")
(set-face-background 'todo-open-3-face "cyan")

(setq todo-closed-face (make-face 'todo-closed-face))
(set-face-foreground 'todo-closed-face "black")
(set-face-background 'todo-closed-face "green")

(setq todo-font-lock-keywords
 '(
   ("BOGUS.*__" . todo-open-0-face)
   ("#~T0\\S *" . todo-open-0-face)
   ("#~T1\\S *" . todo-open-1-face)
   ("#~T2\\S *" . todo-open-2-face)
   ("#~T3\\S *" . todo-open-3-face)
   ("#~T\\.\\S *" . todo-closed-face)
   ("#~K\\.\\S *" . todo-closed-face)
   ))

(defun todo-mode-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(todo-font-lock-keywords t))
  (font-lock-mode)
  )

(add-hook 'todo-mode-hook 'todo-mode-font-lock)

(defun todo-mode () (interactive)
  (run-hooks 'todo-mode-hook)
  )

;;; removed some imenu stuff
