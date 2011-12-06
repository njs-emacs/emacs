(defvar default-format nil nil)

(make-variable-buffer-local 'default-format)
(setq-default default-format "%s")
 
(defun prino (x &optional format)
  "Make OBJECT into a string, if not already, by formatting with
optional FORMAT (default \"%s\")"
  (cond ((or (not (stringp x)) format) (format (or format default-format) x))
	(x)))

(defun printf (&rest args) (princ (apply 'format args)))

(fset 'prin 'prino)

"Format LIST, using FORMAT for list members"
(defun prin* (x format)
  (apply 'format format x))

"Format STRING as a quoted string, but if nil, replace with NIL-REP"
(defun prinq (x &optional nil-rep)
  (concat "\"" (or x nil-rep) "\""))

"Make OBJECT into a string, but if nil, replace with NIL-REP"
(defun prins (x &optional nil-rep)
  (cond (x (prino x)) ((or nil-rep ""))))

"Make LIST into a list of strings formatted using FORMAT"
(defun mprin (list &optional format)
  (mapcar '(lambda (x) (prino x format)) list))

"Make LIST into a list of strings formatted using FORMAT"
(defun mprin* (list &optional format)
  (mapcar '(lambda (x) (prin* x format)) list))

(defun \ cat (fun)
  (mapconcat '(lambda (x) (funcall fun x format)) list (or sep "")))

(defun cat (list &optional sep format)
  (\ cat 'prino))

(defun cats (list &optional sep format)
  (\ cat 'prins))

(defun cat* (list &optional sep format)
  (\ cat 'prin*))

(defun catq (list &optional sep format)
  (\ cat 'sprint))

(defun %% (x &optional arg)
  (let ((fun (intern (concat "%%" (prino arg)))))
    (cond ((fboundp fun) (funcall fun x))
	  ((prino x)))
    ))

(defun %%1 (x)
  (cat x "\n"))

(defun %%2 (x)
  (cat (mapcar '%%1 x) "\n\n"))

(defmacro docat (var list sep &rest body)
  `(mapconcat '(lambda (,var) ,@body) ,list ,sep))

"Bind VAR to each element of LIST in turn and build a list of 
strings formatted using FORMAT. The remaining args are the parameters
to be passed to format"
(defmacro doprin (var list format &rest args)
  `(mapcar '(lambda (,var)
	      (format ,format ,@args)) ,list))

;;; new

(defun format-if (format object) (and object (format format object)))
