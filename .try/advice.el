(qb-define (kbd "C-1") ".emacs.el" t)

(defun a-qb-define (key form expand) (debug))
(defun ar-qb-define (fun &rest args) (debug))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; add-function works on a PLACE, not a symbol
; https://www.gnu.org/software/emacs/manual/html_node/elisp/Core-Advising-Primitives.html#Core-Advising-Primitives

(add-function :before (symbol-function 'qb-define) 'a-qb-define)
(remove-function (symbol-function 'qb-define) 'a-qb-define)

(add-function :around (symbol-function 'qb-define) 'ar-qb-define)
(remove-function (symbol-function 'qb-define) 'ar-qb-define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  (define-advice 'qb-define (:around ()))

(advice-add 'qb-define :around 'ar-qb-define)
