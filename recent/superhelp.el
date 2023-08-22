;;; superhelp.el --- More help than you can handle

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Nick Steed <nick@otzo.org>
;; Version: 0.2
;; Package-Requires: ((hydra "0.15.0"))
;; Keywords: emacs, hydra, help
;; URL: https://example.com/jrhacker/superfrobnicate

;;; Commentary:

;; This package provides a hydra to access all possible help functions
;; so that you can move around a buffer containing symbols and activate
;; whatever help functions you want on the thing at point.

(setq superhelp-base-map (make-sparse-keymap))

(dotimes (i 26)
  (define-key superhelp-base-map (char-to-string (+ ?a i)) 'hydra-keyboard-quit)
  )
(define-key superhelp-base-map "n" 'next-line)
(define-key superhelp-base-map "p" 'previous-line)

(defun symbol-unquote (sym)
  "Remove any prepended quote to symbol, like what symbol-at-point can do."
  (let ((name (symbol-name sym)))
    (intern-soft (replace-regexp-in-string "^'*" "" name))
    )
  )
 
(defun symbol-unquote-at-point ()
  (symbol-unquote (symbol-at-point))
  )
 
(defun symbol-nearby ()
  "Like symbol-at-point, but will try to move to the nearest sensible symbol."
  (sx
   (cond
    ((looking-at "\\sw"))
    ((and (looking-at "\\s *$") (rsb "\\sw" (point^))))
    ((rsf "\\sw"))
    )
   (symbol-unquote-at-point)
   )
  )
 
(defun s-describe-function ()
  "Accelerate describe-function: run on symbol-nearby with no user input."
  (interactive)
  (describe-function (symbol-nearby))
  )
  
(defun s-describe-variable ()
  "Accelerate describe-variable: run on symbol-nearby with no user input."
  (interactive)
  (describe-variable (symbol-nearby))
  )
  
(defun s-describe ()
  "Accelerate symbol help: run either describe-variable or describe-function
on symbol-nearby with no user input."
  (interactive)
  (let ((sym (symbol-nearby)))
    (cond
     ((fboundp sym) (describe-function sym))
     ((boundp sym) (describe-variable sym))
     )
    )
  )

(defun s-where-is ()
  "Accelerate where-is: run on symbol-nearby with no user input."
  (interactive)
  (let ((sym (symbol-nearby)))
    (cond
     ((fboundp sym)
      (let* ((keys
	     (mapcar 'key-description (where-is-internal sym)))
	     (s (mconcat keys "||"))
	     )
	(kill-new s)
	(cond (superhelp-insert (insert s)))
	)
      (where-is sym)
      )
     ((message "%s is not a function" sym))
     )
    )
  )
  
(defun s-call-interactively ()
  "Interactively call symbol-nearby function."
  (interactive)
  (let ((sym (symbol-nearby)))
    (cond
     ((commandp sym) (call-interactively sym))
     ((message "%s is not an interactively callable function" sym))
     )
    )
  )
  
(defun s-apropos ()
  "Give us a quick apropos of symbol-nearby"
  (interactive)
  (let ((sym (or (region-text) (symbol-name (symbol-nearby)))))
    (apropos sym)
    )
  )
  
(defun s-completions ()
  "Show completions of symbol-nearby"
  (interactive)
  (let* ((sym (or (region-text) (symbol-name (symbol-nearby))))
	 (fun (completing-read
	       "Describe: "
	       obarray 'fboundp t sym nil
	       sym)))
    )
  )

(defun region-symbol () (let ((text (region-text))) (and text (intern text))))

(defun s-def-key (key)
  "Map key to symbol-nearby"
  (interactive "KKey: ")
  (let* ((sym (or (region-symbol) (symbol-nearby))))
    (debug)
    (define-key global-map key sym)
    )
  )

(defun copy-symbol-nearby () (interactive)
  (let ((s (symbol-name (symbol-nearby))))
    (kill-new s)
    (message "Copied '%s' to kill" s)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-hydra-superhelp (&rest plist)
  (let ((color (plist-get plist :color)))
    `(defhydra ,(intern (format "hydra-superhelp-%s" color))
       ,(append '(
		  :hint nil
;			:pre (message "pre")
;			:post (message "post")
		  	:base-map superhelp-base-map
			:timeout 10
			)
		plist)
      "
_c_:   Auto		_a_:   Apropos		_k_:   Key
_f_:   Function		_x_:   Execute		_q_:   Quit
_v_:   Variable		_t_:   Completions	_p_:   Copy
_b_:   Binding		_m_:   Map
_i_:   Insert %`superhelp-insert
_s_:   Select
"
  ("t" (s-completions))

  ("c" (s-describe))
  ("f" (s-describe-function))
  ("v" (s-describe-variable))

  ("b" (s-where-is))
  ("w" (s-where-is))
;  ("k" (s-where-is))

  ("a" (s-apropos))
  ("t" (s-completions))

  ("e" (s-call-interactively) :color blue)
  ("x" (s-call-interactively) :color blue)

  ("k" describe-key)
  ("m" s-def-key)

  ("p" copy-symbol-nearby :color blue)
  ("i" (flip superhelp-insert) :color pink)

  ("s" avy-goto-symbol-1)

  ("q" nil :color blue)
  )))

(def-hydra-superhelp :color blue :foreign-keys run)
(def-hydra-superhelp :color pink)

;;;(defhydra hydra-superhelp (:color pink)
;;;  "Super Help"
;;;  ("c" (s-describe) "Auto" :column "Action:")
;;;  ("f" (s-describe-function) "Function")
;;;  ("v" (s-describe-variable) "Variable")
;;;  ("b" (s-where-is) "Binding")
;;;  ("q" nil "Quit" :color blue)
;;;  )

(defun hydra-superhelp-blue/enter (&optional arg)
  (interactive "P")
  (setq superhelp-insert nil)
  (hydra-superhelp-blue/body)
  )

(defun hydra-superhelp-pink/enter (&optional arg)
  (interactive "P")
  (setq superhelp-insert nil)
  (hydra-superhelp-pink/body)
  )

(define-key global-map (kbd "H-#") 'hydra-superhelp-blue/enter)
(define-key global-map (kbd "H-~") 'hydra-superhelp-pink/enter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	hydra-base-map		
;;	hydra-keyboard-quit
;; 	hydra-superhelp-blue/enter	
;; 	hydra-superhelp-blue/enter	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * completion for superhelp
;;  (complete-with-action nil obarray "view" '(lambda (x) t))
;;  (completion-all-completions "view" obarray 'fboundp 4)
;;  (completion-all-completions "vie-lo" obarray 'fboundp 6)

(provide 'superhelp)

;;; superhelp.el ends here
