; sync-trial (a b n) finds the first match of length n of b in a, 
; and a in b

(defun sync-trial-safe (a b n)
  (let* ((ia (strstr a (substring b 0 n) (* n 2)))
	 (ib (strstr b (substring a 0 n) (* n 2)))
	 )
    (cond (ia (list ia ib (strdiff b (substring a ia))))
	  (ib (list ia ib (strdiff a (substring b ib))))
	  ((sync-trial-safe a b (/ n 2)))
	  )
    ))

(defun sync-trial (a b n)
  (sync-trial-safe a b (min n (length a) (length b))))

(defun sync (a b)
  (let* ((i (strdiff a b))
	 (n (- (min 8 (length a) (length b)) i))
	 (ii (sync-trial-safe (substring a i) (substring b i) n))
	 )
    (list i ii)))

(defun check-insert (a b i n)
  (let ((ia (strstr a (substring b i n)))
	(ib (strstr b (substring a i n)))
	)
    (list ia ib)
    ))

(check-insert ",,,,defghij      " "...defghij       " 3 4)

(sync-trial "abcdefg" " abcdef" 35)

(sync "abcdef12345.78" "abc...def12345678")

(sync "abc,,,defghij      " "abc...defghij       ")

(strdiff "ab" "abc")

(defun string-diff-index (a b)
  (and (not (string-equal a b))
       (let ((len (min (length a) (length b)))
	     (i 0))
	 (while (and (< i len) (= (aref a i) (aref b i))) (setq i (1+ i)))
	 i)))

(defun string-diff-pair (a b &optional align)
  (let ((i (string-diff-index a b)))
    (and i (setq i (logand i (lognot (1- (or align 1)))))
	 (list (substring a i) (substring b i)))))

(defun string-sync-try (a b len)
  (let* ((i (string-match (substring a 0 len) b))
	 )
    (and i
	 (let* ((b (substring b i))
		(min (min (length b) (length a))))
	   (while (and (< len min)
		       (= (aref a len) (aref b len)))
	     (setq len (1+ len)))
	   (list i len)))))

(defun string-sync (a b &optional len)
  (let* ((len (min (or len 64) (length a) (length b)))
	 (ia (string-sync-try a b len))
	 (ib (string-sync-try b a len))
	 )
    (cond
     ((and ia (or (not ib) (<= (car ia) (car ib)))) (cons 'ins ia))
     ((and ib (or (not ia) (< (car ib) (car ia)))) (cons 'del ib))
     ((<= len min-sync) nil)
     ((string-sync a b (/ len 2))))))

(setq string-diff-merge t)

(defun mod-up (i mod) (* (/ (+ i mod -1) mod) mod))

(defun string-diff (a b &optional min-sync offset)
  (let* ((offset (or offset 0))
	 (min-sync (or min-sync 4))
	 (lena (length a))
	 (lenb (length b))
	 (min (min lena lenb))
	 (s (string-sync a b))
	 )
    (cond
     ((and (zerop lena) (zerop lenb)) nil)
     ((zerop lena) (list (list offset nil b)))
     ((zerop lenb) (list (list offset a nil)))
     ((not s)
      (let ((i 0) (aa a) (bb b))
	(while (and (not s) (< i min))
	  (setq a (substring a 1))
	  (setq b (substring b 1))
	  (setq i (1+ i))
	  (setq s (string-sync a b)))
	(let* ((rest (string-diff a b min-sync (+ offset i)))
	       (first (car rest)))
	  (cond
	   ((and string-diff-merge first
		 (null (nth 1 first))
		 (= (nth 0 first) (+ offset i)))
	    (cons (list offset
			(substring aa 0 i)
			(concat (substring bb 0 i) (nth 2 first))) (cdr rest)))
	   ((and string-diff-merge first
		 (null (nth 2 first))
		 (= (nth 0 first) (+ offset i)))
	    (cons (list offset
			(concat (substring aa 0 i) (nth 1 first))
			(substring bb 0 i)) (cdr rest)))
	   ((cons (list offset
			(substring aa 0 i) (substring bb 0 i)) rest))))
	))
     ((let ((o (nth 1 s))
	    (l (nth 2 s)))
	(cond
	 ((eq o 0) (string-diff
		    (substring a l) (substring b l) min-sync (+ offset l)))
	 ((if (eq (nth 0 s) 'ins)
	      (cons (list offset nil (substring b 0 o))
		    (string-diff
		     (substring a l) (substring b (+ o l)) min-sync
		     (+ offset l o))
		    )
	    (cons (list offset (substring a 0 o) nil)
		  (string-diff
		   (substring a (+ o l)) (substring b l)
		   min-sync (+ offset l))))
	 ))
       )))
    ))

(defun hstring-diff (a b)
  (autoload 'string-to-hex-string "hex.el")
  (let* ((ha (string-to-hex-string a))
	 (hb (string-to-hex-string b))
	 (diff (string-diff ha hb 2))
	 )
    (mapcar '(lambda (x) (list
			  (* 2 (car x))
			  (hex-string-to-string (nth 1 x))
			  (hex-string-to-string (nth 2 x)))) diff)))
