;; includes functions without which any phase of init may fail.

(defun defin (fun &optional hook)
  (interactive (list (function-called-at-point)))
  (put fun 'lisp-indent-function (or hook 'defun)))

(defmacro save-search (&rest body)
  "Execute body, saving and restoring results of previous search."
  `(let ((save (match-data)))
     (prog1 (progn ,@body) (store-match-data save))))

;;; (canonical-function 'string-sub)

(defun string-sub (string old new)
  "Substitute in STRING, all occurrences of regexp OLD with string NEW.
If STRING is a symbol, then it is converted to a string.
NEW will be evalled."
  (if (symbolp string) (setq string (symbol-name string)))
  (save-search
   (let (out index)
     (while (setq index (string-match old string))
       (setq out (concat out (substring string 0 index) (eval new)))
       (setq string (substring string (match-end 0))))
     (concat out string))))

(defun filename-clean (x)
  (setq x (string-sub x "\\\\" "/"))
  (concat (substring x 0 1) (string-sub (substring x 1) "/+" "/"))
  )

(defun filename-canonical (x &optional dir)
  (filename-clean
   (expand-file-name
    (cond
     (dir (concat dir "/" x))
     (x)
     )))
  )
  
(defun filename-concat (&rest list)
  (let ((s (cat list "/")))
    (filename-clean s)
    )
  )

(defun filename-format (&rest args)
  (filename-clean (apply 'format args))
  )

(fset '@fc 'filename-canonical)

(defun concat-path (&rest list) (filename-clean (mconcat list "/")))

