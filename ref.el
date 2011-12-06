;
;  Functions to process document references
;

(defun unref (s) "Make a reference string into a list"
  (unconcat s "\\."))

(defun ref< (a b) "Return true if reference A comes before reference B."
  (catch 'result
    (let* ((a (unref a))
	   (b (unref b))
	   )
      (while a
	(let* ((a (car a))
	       (b (car b)))
	  (cond ((less a b) (throw 'result nil))
		((less b a) (throw 'result t))
		))
	(setq a (cdr a))
	(setq b (cdr b)))
      (throw 'result (and b t)))))

