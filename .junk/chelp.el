; obsolete
; interesting concept - shortcuts to help based on buffer context
; related to the M-g search -> grep
; but can relate to thing-at-point
;
; this was related to e:/daily/1104/14/chelp.pl
; which had selective primitive highlighting of a key string
; which can also relate to font-lock inside help text etc
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chelp () (interactive)
; (shell-command-on-region (mark) (point) (format "perl chelp.pl"))
 (shell-command-on-region (sxp (bol)) (sxp (eol)) (format "perl chelp.pl"))
 (other-window 1)
 (cond ((rsf "##here##") (bol) (kill-line 1)))
 )

(define-key global-map "\C-z\C-h" 'chelp)
