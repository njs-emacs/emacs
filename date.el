(autoload 'perm "random.el")

(defun num-list-less-p (a b)
  (let ((ca (car a)) (cb (car b)))
    (and ca
	 (or (< ca cb)
	     (and (= ca cb) (num-list-less-p (cdr a) (cdr b)))))))

(defun date-less-p (a b)
  (let ((a (mapcar 'string-to-int (unconcat a "[/-]")))
	(b (mapcar 'string-to-int (unconcat b "[/-]")))
	(perm '(2 0 1)))
    (mapcar '(lambda (x)
	       (and (> (nth 2 x) 1900)
		    (setcar (nthcdr 2 x) (- (nth 2 x) 1900))))
	    (list a b))
    (num-list-less-p (perm a perm) (perm b perm))
    ))

(defun toggle-year () (interactive)
  (let ((now (call-shell "date +%y%m%d%H%M%S")))
    (call-shell (format "su root -c \"date %s%s.%s\""
			(if (string-match "^94" now) "91" "94")
			(substring now 2 -3)
			(substring now -3 -1)
			))))
