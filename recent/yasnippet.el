;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; once snippets are defined here they can be converted into
;; snippet files using yas-describe-tables and clicking on the name
;; there isn't a simple function to define a single snippet
;; so here is one

(defun yas-define-one-snippet (key template &rest args)
  (let* ((mode (or (plist-get args ':mode) major-mode))
	 (kargs
	  (mapcar '(lambda (x) (plist-get args x))
		  '(:name :condition :group :expand-env :load-file :keybinding :uuid :save-file)))
	 (x `(,key ,template ,@kargs))
	 (snippets (list x))
	 )
    (yas-define-snippets mode snippets)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; There doesn't seem to be a way of removing yasnippets once
;;; defined. A snippet can be disabled by expanding it to itself
;;; and removing the keybinding, or mapping a different snippet
;;; to that binding

;;; (yas-define-one-snippet "poop" "this is a poop $0 !" :keybinding "A-w" :mode 'perl-mode)
;;; (yas-define-one-snippet "poop" "poop")



