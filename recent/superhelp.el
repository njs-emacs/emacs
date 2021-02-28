(setq superhelp-base-map (make-sparse-keymap))

(dotimes (i 26)
  (define-key superhelp-base-map (char-to-string (+ ?a i)) 'hydra-keyboard-quit)
  )
(define-key superhelp-base-map "n" 'next-line)
(define-key superhelp-base-map "p" 'previous-line)


(defun s-describe-function ()
  "Accelerate describe-function: run on symbol-at-point with no user input."
  (interactive)
  (describe-function (symbol-at-point))
  )
  
(defun s-describe-variable ()
  "Accelerate describe-variable: run on symbol-at-point with no user input."
  (interactive)
  (describe-variable (symbol-at-point))
  )
  
(defun s-describe ()
  "Accelerate symbol help: run either describe-variable or describe-function
on symbol-at-point with no user input."
  (interactive)
  (let ((sym (symbol-at-point)))
    (cond
     ((fboundp sym) (describe-function sym))
     ((boundp sym) (describe-variable sym))
     )
    )
  )
  
(defun s-where-is ()
  "Accelerate where-is: run on symbol-at-point with no user input."
  (interactive)
  (let ((sym (symbol-at-point)))
    (cond
     ((fboundp sym) (where-is sym))
     ((message "%s is not a function" sym))
     )
    )
  )
  
(defun s-call-interactively ()
  "Interactively call symbol-at-point function."
  (interactive)
  (let ((sym (symbol-at-point)))
    (cond
     ((commandp sym) (call-interactively sym))
     ((message "%s is not an interactively callable function" sym))
     )
    )
  )
  
(defun s-apropos ()
  "Give us a quick apropos of symbol-at-point"
  (interactive)
  (let ((sym (or (region-text) (symbol-name (symbol-at-point)))))
    (apropos sym)
    )
  )
  
(defun s-completions ()
  "Show completions of symbol-at-point"
  (interactive)
  (let* ((sym (or (region-text) (symbol-name (symbol-at-point))))
	 (fun (completing-read
	       "Describe: "
	       obarray 'fboundp t sym nil
	       sym)))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-superhelp (:hint nil :color pink :base-map superhelp-base-map)
      "
_c_:   Auto		_a_:   Apropos		_k_:   Key
_f_:   Function		_x_:   Execute		_q_:   Quit
_v_:   Variable		_t_:   Completions
_b_:   Binding
"
  ("t" (s-completions))

  ("c" (s-describe))
  ("f" (s-describe-function))
  ("v" (s-describe-variable))

  ("b" (s-where-is))
  ("w" (s-where-is))
  ("k" (s-where-is))

  ("a" (s-apropos))

  ("e" (s-call-interactively) :color blue)
  ("x" (s-call-interactively) :color blue)

  ("k" describe-key)

  ("q" nil :color blue)
  )

;;;(defhydra hydra-superhelp (:color pink)
;;;  "Super Help"
;;;  ("c" (s-describe) "Auto" :column "Action:")
;;;  ("f" (s-describe-function) "Function")
;;;  ("v" (s-describe-variable) "Variable")
;;;  ("b" (s-where-is) "Binding")
;;;  ("q" nil "Quit" :color blue)
;;;  )

(define-key global-map (kbd "H-#") 'hydra-superhelp/body)

; hydra-base-map
; hydra-keyboard-quit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * completion for superhelp
;;  (complete-with-action nil obarray "view" '(lambda (x) t))
;;  (completion-all-completions "view" obarray 'fboundp 4)
;;  (completion-all-completions "vie-lo" obarray 'fboundp 6)

;;  vie-lo
;;  view
