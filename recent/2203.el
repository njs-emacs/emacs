(defmacro flipp (symbol)
   "Toggle SYM. Echos the new value, and returns the new value"
  `(prog1
       (setq ,symbol (not ,symbol))
     (message "%s is set to %s" (quote ,symbol) ,symbol)
     )
  )

(defmacro flip (symbol)
   "Toggle SYM. Does not return the value, but just echos the new value"
  `(progn
     (setq ,symbol (not ,symbol))
     (message "%s is set to %s" (quote ,symbol) ,symbol)
     )
  )

(flip case-fold-search)

(define-key org-mode-map (kbd "<H-return>") 'eval-defun-without-narrow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro setq-push (sym value)
  (let* ((ssym (intern (format "-%s--stack" sym))))
    (or (boundp ssym) (set ssym nil))
    (or (boundp sym) (set sym nil))
    `(let ()
       (setq ,ssym (cons ,sym ,ssym))
       (setq ,sym ,value)
       )
    )
  )

(defmacro setq-pop (sym)
  (let* ((ssym (intern (format "-%s--stack" sym)))
	 (ssv (and (boundp ssym) (symbol-value ssym)))
	 )
    (cond
     (ssv
      (set sym (car ssv))
      (set ssym (cdr ssv))
      `,sym
      )
     (
      `(message "stack is empty for '%s'" ',sym))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; these functions are used as a quick way of interactively
; selecting an argument to a function not being called interactively
; we can define alternate interactive argument reading methods

; call-interactively

(defun one-of (list &rest plist)
  "Use completing-read to select one item from a list of symbols"
  (cond
;   ((stringp list) (apply 'one-of (mapcar 'intern-soft (unconcat list)) plist))
   ((stringp list) (apply 'one-of (unconcat list) plist))
   ((let* (
	   (default (or (plist-get plist :default) (car list)))
	   (initial (plist-get plist :initial))
	   (predicate (plist-get plist :predicate))
	   (prompt (format (or (plist-get plist :prompt) "Choose (%s): ") default))
	   )
      (completing-read prompt list predicate t initial nil default)
      )
    )
   )
  )

(defun one-of* (&rest list)
  "quick shortcut to one-of (without plist)"
  (one-of list)
  )

(defmacro one-of** (&rest list)
  "macro shortcut to one-of (without plist)"
  `(one-of ',list)
  )

(defun one-of-eval (list &rest plist)
  "Use completing-read to select one item from a list"
  (eval (apply 'one-of list plist))
  )

;;; (eval (one-of `(ofs elfs)))
;;; (one-of-eval `(ofs elfs))
;;; 
;;; (one-of** ofs elfs)
;;; 
;;; (quote x)
;;; 
;;; (one-of* 'ofs 'elfs)
;;; (one-of `(ofs elfs))
;;; (one-of `("abc" "def"))
;;; (one-of "abc def")
;;; 
;;; (one-of "a b c")
;;; (one-of `(a b c cde xyz))
;;; 
;;; (mapcar 'prin1-to-string '(a b c))
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun alt-interactive (function spec)
  "Call an interactive function using an alternate interactive spec"
  (let* ((args (cadr (symbol-function function)))
	 (fun `(lambda (&rest args) (interactive ,spec) (apply ',function args)))
	 )
    (call-interactively fun)
    )
  )

;;; (alt-interactive 'find-file "FGo: ")
