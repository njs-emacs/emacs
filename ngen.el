(defun ngen-buffer-name () " *ngen*")
(defun ngen-buffer () (get-buffer-create (ngen-buffer-name)))

(defun ngen-find (plist)
  (let* ((buffer (ngen-buffer))
	 (a (alist-slice plist '(file cycle)))
	 (s (format ":%S:" a))
	 (pos)
	 )
    (with-current-buffer buffer
      (bob)
      (setq pos (search-forward s nil t))
      pos
      )
    )
  )

(defun ngen-cache-force (plist)
  (let* ((buffer (ngen-buffer))
	 (alist (alist-slice plist '(file cycle)))
	 (args (alist-to-args alist))
	 (names)
	 )
    (setq names
      (shell-command-to-string(concat "perl e:/perl/lib/names/name.pl" " " args))
      )
    (with-current-buffer buffer
      (bob)
      (insert (format ":%S:\n" (alist-sort alist)))
      (insert names)
      )
    )
  )

(defun ngen-cache (plist)
  (let* ((found))
    (setq found (ngen-find plist))
    (cond
     (found)
     ((ngen-cache-force plist))
     )
    )
  )

(defun ngen-read-cache (line plist)
  (with-current-buffer (ngen-buffer)
    (let* ((found))
      (ngen-cache plist)
      (setq found (ngen-find plist))
      (fl line)
      (bol)
      (bs (point^) (point$))
      )
    )
  )

(defun ngen-read (line state)
  (let* ((method (alist-get 'method state)))
    (cond
     (method (funcall method line state))
     (t (ngen-read-cache line state))
     )
    )
  )

(defun ngen-get (tag) (alist-get tag ngen-state))
(defun ngen-put (tag val) (setq ngen-state (alist-put ngen-state tag val)))

(defun ngen-current-move (val) (ngen-put 'current (+ (ngen-get 'current) val)))

(defvar-local ngen-state nil "Name generation state")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ngen-current (&optional offset)
  (cond
   (ngen-state
    (let* ((offset (or offset 0))
	   (line (ngen-get 'current))
	   (text (ngen-read (+ line offset) ngen-state))
	   )
      text)
    )
   ("")
   )
  )

(defun ngen-insert-current (offset) (interactive "P")
  (insert (ngen-current offset))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zunk (line state)
  (format (or (alist-get 'format state) "%d") line)
  )

(defun zunka (line state)
  (let* ((format (or (alist-get 'format state) "%s"))
	 (s
	  (cond
	   ((< line 26) (format "%c" (+ line ?A)))
	   )
	  )
	 )
    (format format s)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra ngen-hydra (:pre nil :post nil)
  "Names\n"

  ("p" (ngen-insert-current 0) (format "Current (%s)" (ngen-current)) :exit t :column "History:")

  ("o" (ngen-insert-current -1) (format "     -1 (%s)" (ngen-current -1)) :exit t)
  ("i" (ngen-insert-current -2) (format "     -2 (%s)" (ngen-current -2)) :exit t)
  ("u" (ngen-insert-current -3) (format "     -3 (%s)" (ngen-current -3)) :exit t)

  ("[" (ngen-current-move -1) (format "Back    -> %-12s" (ngen-current -1)) :column "Move:")
  ("]" (ngen-current-move 1)  (format "Forward -> %-12s" (ngen-current  1)))

  ("q" nil "quit" :column ":")
  )

(define-key global-map (kbd "M-n M-n") 'ngen-insert-current)
(define-key global-map (kbd "M-n M-m") 'ngen-hydra/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ngen-state-insert ()
;;; needs to detect and replace old version
;;; needs to run as file save hook
  (interactive)
  (insert (format "::ngen:: :%S:\n" (alist-sort ngen-state)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ngen-state-read ()
;;; needs to detect if missing
;;; needs to be run as file open hook
 (interactive)
  (sx
   (bob)
   (rsf "::ngen:: :\\(.*\\):")
   (let* ((s (ms 1))
	  (s (read s))
	  )
     (setq ngen-state s)
     )
   )
  )

