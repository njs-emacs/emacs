(defun wakeup (sym n f)
  (let (proc)
    (kill-and-remove-process (get sym 'process))
    (setq proc (start-process (symbol-name sym) nil
			     (concat exec-directory "wakeup")
			     (format "%d" n)))
    (set-process-filter proc f)
    (put sym 'process proc)))

(defun kill-wakeup (sym)
  (let ((proc (get sym 'process)))
    (if proc (kill-process proc))))

(defun make-one-line-window (buf)
  (let (old win)
    (setq old (selected-window))
    (while (not (top-window-p)) (other-window 1))
    (setq win (selected-window))
    (if (> (window-height win) 2) (split-window-vertically 2))
    (select-window win)
    (switch-to-buffer buf)
    (select-window old)))

(kill-wakeup 'doit)

(wakeup 'doit 300
	'(lambda (proc x)
	   (make-one-line-window (get-buffer-create "doit"))
	   (beep)
	   (save-excursion (set-buffer "doit")
			   (erase-buffer)
			   (insert (doit)))))

(defun doit ()
  (random-nth (list
	       (format "%d situps" (random-rng 5 10))
	       (format "%d pushups" (random-rng 5 10))
	       (format "%d stairs" (random-rng 4 6))
	       (format "%d secs bike" (* 30 (random-rng 2 4)))
	       (format "%d knee bends" (random-rng 5 10))

	       )
   ))


