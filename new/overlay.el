(overlay-put foo 'modification-hooks
	     '((lambda (&rest args) (message "foo %s" args))))
(overlay-put foo 'insert-in-front-hooks
	     '((lambda (&rest args) (message "ins< %s" args))))
(overlay-put foo 'insert-behind-hooks
	     '((lambda (&rest args) (message "ins> %s" args))))
(overlay-put foo 'before-string "<<")
(overlay-put foo 'after-string ">>")
