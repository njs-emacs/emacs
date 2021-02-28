(defun lol-nextrec ()
  (re-search-forward "K\\([0-9]+\\)" nil 1) (bol)
;  (message "nextrec %s" (point))
;  (point)
  )

(defun lol-endrec ()
  (lol-nextrec) (fl -1) (eol)
;  (message "endrec %s" (point))
;  (point)
  )

;;; 
; if startkey returns nil, the current point is used
; but if startkey returns (point) the endkey function doesn't get called

(defun lol-startkey ()
  (rsf "K\\([0-9]+\\)" nil 1 1 t)
;  (message "startkey %s" (point))
;  (point)
  (ms 1)
  )

(defun lol-endkey ()
  )

(defun lol-compare (a b)
;  (message (format "[%s] [%s]" a b))
  (string< a b)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lol-startkey ()
  (rsf "K\\([0-9]+\\)" nil 1 1 t)
;  (message "startkey %s" (point))
  nil
  )

(defun lol-endkey ()
  (goto-char (me 1))
;  (message "endkey %s" (point))
  nil
  )

(defun lol-compare (a b)
;  (message (format "[%s] [%s]" a b))
  (string< (bs (car a) (cdr a)) (bs (car b) (cdr b)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fdfd () (interactive)
       (sx
 (sort-subr nil
  'lol-nextrec
  'lol-endrec
  'lol-startkey
  'lol-endkey
  'lol-compare
  )
  ))

(sort-build-lists 'nextrec 'endrec 'startkey 'endkey)

===============================================================
K99
 k99
 k99
## (99)
heres K0
 k0
 k0
 k0
## (0)
Last is K8
 k8
 k8
 k8
 k8
## (1)

Last is K1
 k1
 k1
 k1
 k1
## (1)

unsorted A

And K2
 k2
 k2
 k2
 k2
## (2)
here's K3
 k3
 k3
 k3
## (3)

unsorted B
