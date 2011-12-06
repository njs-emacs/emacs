(defun node (contents &optional left right)
  (list contents left right))

(defun print-tree (node &optional fun)
  (or fun (setq fun 'prins))
  (if (not node) "nil"
    (let ((l (print-tree (nth 1 node) fun))
	  (r (print-tree (nth 2 node) fun))
	  (in (funcall fun (car node))))
       (concat "(" (mapconcat 'identity (list in l r) " ") ")"))))

(defun print-tree (node &optional f a)
  (or f (setq f 'prins))
  (or a (setq a '(lambda (in l r)
		   (concat "(" (mapconcat 'identity (list in l r) " ") ")"))))
  (if (not node) "nil"
    (let ((l (print-tree (nth 1 node) f a))
	  (r (print-tree (nth 2 node) f a))
	  (in (funcall f (car node))))
       (funcall a in l r))))

(defun traverse-pre (node fun)
  (and node
       (let ((l (traverse-pre (nth 1 node) fun))
	     (r (traverse-pre (nth 2 node) fun))
	     (in (list (funcall fun node))))
       (append in l r))))

(defun traverse-post (node fun)
  (and node
       (let ((l (traverse-post (nth 1 node) fun))
	     (r (traverse-post (nth 2 node) fun))
	     (in (list (funcall fun node))))
       (append l r in))))

(defun traverse-in (node fun)
  (and node
       (let ((l (traverse-in (nth 1 node) fun))
	     (r (traverse-in (nth 2 node) fun))
	     (in (list (funcall fun node))))
       (append l in r))))

'(let ((node
       (node 'a
	     (node 'L)
	     (node 'R nil
		   (node 'RR
			 (node 'RRL)
			 (node 'RRR)))))
      (fun '(lambda (node) (format "<%s>" (car node)))))
  (list (traverse-pre node fun)
	(traverse-post node fun)
	(traverse-in node fun)))
	
	      
