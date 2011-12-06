(defun random-mod (n) (mod (abs (random)) n))
(defun random-rng (lo hi) (+ lo (random-mod (- hi lo))))
(defun random-p (n from) (let ((r (random-mod from))) (<= n r)))
(defun random-nth (list) (nth (random-mod (length list)) list))

(defun random-list (length list)
  (let ((n (length list)) out)
    (while (> (*-- length) 0)
      (setq out (cons (nth (random-mod n) list) out)))
    out))

(defun perm-i (n &optional base)
  "Return identity permutation with length N. Optional argument BASE starts
permutation at that value."
  (let (p (base (or base 0)))
    (setq n (+ n base))
    (while (> n base)
      (setq n (1- n)) (setq p (cons n p)))
    p))

(defun perm-swap (list a b)
  (let ((save (nth a list)))
    (setcar (nthcdr a list) (nth b list))
    (setcar (nthcdr b list) save))
  list)

(defun perm-rq (list)
  "Create a random permutation of the elements of LIST.
The list is modified by side-effect."
  (let* ((n (length list))
	 (i n))
    (while (> i 0)
      (setq i (1- i)) (perm-swap list i (random-mod n))))
  list)

(defun perm-p (nt nn)
  "Create a random permutation of NT values of t, and NNIL values of nil."
  (perm-rq (append (make-list nt t) (make-list nn nil))))

(defun perm (list perm)
  "Create a permutation of the elements of LIST, controlled through PERM."
  (mapcar '(lambda (x) (nth x list)) perm))

(defun perm-r (list)
  "Create a random permutation of the elements of LIST."
  (perm list (perm-rq (perm-i (length list)))))

(defun merge (a b perm)
  (let (result)
    (while perm
      (setq result (cons (if (car perm)
			     (prog1 (car a) (setq a (cdr a)))
			   (prog1 (car b) (setq b (cdr b)))) result))
      (setq perm (cdr perm)))
    (nreverse result)))
      
(defun merge-r (lists)
"Merge LISTS at random, preserving the original order of elements in each
of the lists.
For example (merge-r '((a b c) (0 1 2))) -> (a 0 b 1 2 c)
"
  (let ((result (car lists)))
    (while (setq lists (cdr lists))
      (setq result (merge result (car lists)
			  (perm-p (length result) (length (car lists)))))
      )
    result))

(defun rndcat (func cb &optional range)
  (let* ((lo (or (car range) 0))
	 (hi (or (car (cdr range)) 256)))
    (apply 'concat (mdt i cb (funcall func (random-rng lo hi))))))

(defun list-to-string (list) (mapconcat 'char-to-string list ""))

(defun string-r (cb &optional range) (rndcat 'char-to-string cb range))

(defun string-r-set (cb set)
  (setq set (expand-range set))
  (rndcat '(lambda (x)
	     (substring set x (1+ x)))
	  cb (list 0 (1- (length set)))))

(defun expand-range (s)
  (let ((i 0) (len (length s)) result)
    (while (< i len)
      (cond
       ((and (= (aref s i) ?-) (> i 0))
	(let ((start (aref s (1- i)))
	      (end (aref s (1+ i))))
	  (while (< (++ start) end)
	    (setq result (concat result (char-to-string start)))
	    )))
       ((setq result (concat result (char-to-string (aref s i))))))
      (setq i (1+ i)))
    result))


