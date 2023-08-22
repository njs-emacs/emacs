;;; see also e:/help/org/agenda/demo/org-capture-x.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all samples of "d" template is supposed to be a "done" item
;;; added in various ways in various locations
;;; when added as an entry, a property expansion template item will
;;; be added to the property drawer at the indicated location
;;;
;;; what we really want is to have a key which will immediately add
;;; a property to the top level property drawer
;;; the capture template is great but in this case we need something
;;; even more lightweight

;;; (org-capture-template-add "d" '("done" entry (file buffer-file-name) "* done :done:%?\n%T %^{DONE}p|%T"))
;;; (org-capture-template-add "d" '("done" entry (file buffer-file-name) "* done at %T%? :done:\n"))

;;; (org-capture-template-add "d" '("done" entry (file+olp buffer-file-name "shag") "* done at %T%? :done:\n"))
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 0 at %T%? :done:\n"))
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 1 at %<%Y> %? :done:\n"))
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 2 at %<<%Y-%m-%d %H:%M:%S>> %? :done:\n"))
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 3 at %<%Y-%m-%d %H:%M:%S> %<<%Y-%m-%d %H:%M:%S>> %? :done:\n"))
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 4 at %<%H:%M:%S> %<<%Y-%m-%d %H:%M:%S>> %? :done:\n"))
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 5 at %<%H%M%S> %<<%Y-%m-%d %H:%M:%S>> %? :done:\n"))
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 6 at %<%H%M%S> %<<%Y-%m-%d %H:%M:%S>> %? :done: %^G \n%c"))
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 7 %?\n>>>%i"))
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 8 %^{Birthday}t"))

;;; prompts for a property value 'DONE'
;;; (org-capture-template-add "d" '("done" entry (file buffer-file-name) "* done :done:%?\n%T %^{DONE}p|%T"))

;;; the default must be 'allowed' - the %() expression DOES work in this case
;;; (org-capture-template-add "d" '("done" entry (file buffer-file-name) "* foo\n%^{zoo|%(concat \"A\" \"l\")}p" :immediate-finish t))

;;; (org-capture-template-add "d" '("done" plain (file buffer-file-name) "yo\n%^{zoo}p" :immediate-finish t))
;;; (org-capture-template-add "d" '("done" plain (function beginning-of-buffer) "yo\n%^{zoo}p" :immediate-finish t))

;;; no expressions allowed in property default
;;; (org-capture-template-add "d" '("done" entry (file buffer-file-name) "* done :done:%?\n%T %^{DONE|%(format-time-string \"%c\")}p"))
;;; (org-capture-template-add "d" '("done" entry (file buffer-file-name) "* done :done:%?\n%T %^{PONE|%(format-time-string \"%c\")|other}"))
;;; (org-capture-template-add "d" '("done" entry (file buffer-file-name) "* done :done:%?\n%T %^{PONE|%(format-time-string \"%c\")|other}"))


;;; (org-capture-template-add "d" '("done" entry (file buffer-file-name) "%^{PONE|default}p* done %T\n"))

;;; (org-capture-template-add "d" '("done" entry (file buffer-file-name) "%^{boo}p* done %T\n"))

;;; (org-capture-template-add "d" '("audit" entry (id "audit") "%^{boo}p* %T\n")) ;; no
;;; (org-capture-template-add "d" '("audit" entry (file+headline buffer-file-name "audit") "%^{boo}p* %T\n"))
;;; (org-capture-template-add "d" '("audit" plain (file+headline buffer-file-name "audit") "%^{boo}p*"))
;;; (org-capture-template-add "d" '("audit" plain (file+headline buffer-file-name "audit") "%^{boo}p"))

;;; (org-capture-template-add "d" '("audit" plain (file+headline buffer-file-name "audit") " crap\n%^{boo}p" :immediate-finish t))
;;; (org-capture-template-add "d" '("audit" plain (file+headline buffer-file-name "audit") " crap\n%^{boo}p" :immediate-finish t))
;;; (org-capture-template-add "d" '("audit" plain (file+headline buffer-file-name "audit") "* foo\n%^{boo}p" :immediate-finish t))
;;; (org-capture-template-add "o" '("oops" entry (file+headline buffer-file-name "audit") "* yo %^{boo}p" :immediate-finish t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-read-property-value "u" 1 "hello")
(org-read-property-value "CLOSED" 1 "hello")

;; org-special-properties - properties that are not defined in the property drawer, but in some other way.

;; allowed-values is used to validate accepted replies and build completion lists

(org-property-get-allowed-values nil "CLOSED" 'table)
(org-property-get-allowed-values nil "TODO" 'table)

org-property-set-functions-alist

(defun boo--property-read (&optional prompt &rest args)
  (completing-read (or prompt "done at: ") nil nil nil (format-time-string "<%Y-%m-%d %H:%M:%S>"))
  )

(setq org-property-set-functions-alist (alist-put org-property-set-functions-alist "boo" 'boo--property-read))

;;  usually org-set-property-function returns - org-completing-read


;T; (org-set-property-function "boo")
;T; (org-set-property-function "TODO")
;T; (org-set-property-function "boo")

;; actually want to update the property in the target entry, not add new property drawer

(org-set-property-and-value t)  ; use last
(org-set-property-and-value nil)  ; prompt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (org-capture-template-add "d" '("done" plain (function beginning-of-buffer) "yo\n%^{zoo}p" :immediate-finish t))  ;; NO
;;; (org-capture-template-add "d" '("audit" plain (file+headline buffer-file-name "audit") "%^{boo}p"))  ;; OK
;;; (org-capture-template-add "d" '("audit" entry (file+headline buffer-file-name "audit") "* %^{boo}p"))  ;; OK
;;; (org-capture-template-add "d" '("audit" plain (file+headline buffer-file-name "audit") "%^{boo}p" :immediate-finish t))
;;; (org-capture-template-add "d" '("done" plain (file+headline buffer-file-name "audit") "yo\n%^{zoo}p" :immediate-finish t))
;;; (org-capture-template-add "d" '("done" plain (function beginning-of-buffer) "yo\n%^{zoo}p" :immediate-finish t))
;;; (org-capture-template-add "d" '("done" entry (function beginning-of-buffer) "yo\n%^{zoo}p" :immediate-finish t))

;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "yo\n%^{zoo}p" :immediate-finish t)) ;; no
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "yo\n%^{zoo}p" :immediate-finish t)) ;; no
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "yo\n%^{moo}p" :immediate-finish t)) ;; no
;;; (org-capture-template-add "d" '("done" entry (file+headline buffer-file-name "audit") "yo\n%^{moo}p" :immediate-finish t))

;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 8 %^{Birthday}t"))  ;; OK
;;; (org-capture-template-add "d" '("done" entry (function end-of-buffer) "* done 8 %^{Birthday}t" :immediate-finish t)) ;; OK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-capture-templates
  `(
    ("d" "daily" entry (file "daily.org")     "* %? %^G\n")

    ("j" "Journal" entry (file+datetree "diary.org") "* %?\n%U\n" :clock-in t :clock-resume t)

    ("a" "A prefix")
    ("aj" "Jonal" entry (file+datetree "dairy.org") "* %?\n%U\n" :clock-in t :clock-resume t)
    ("ax" "Xonal" entry (file+datetree "dairy.org") "* %?\n%U\n" :clock-in t :clock-resume t)
    ("az" "Zonal" entry (file+datetree "dairy.org") "* NODO %?\n%U\n" :clock-in t :clock-resume t)

    ("t" "todo" entry (file "refile.org") "* TODO %?\n")

;    ("b" "boodo" entry (file+olp "refile.org" "boodoo") "* TODO %?\n")

;    ("s" "smarm" entry (file+olp "refile.org" "smarm") "* TODO %?\n")

;    ("c" "charm" checkitem (file+olp "refile.org" "charm" "bum") "[ ] %?\n")
;    ("x" "Stuff" item (file "refile.org") "%?\n")
;    ("y" "Table 1" table-line (file+olp "sandbox.org" "tables" "1") "| %?  |   | \n"  :table-line-pos "II-1")
;    ("z" "Top 1" entry (file+olp "refile.org" "top 1")  "* %?\n")

    ("n" "note" entry (file "refile.org") "* %? %^G\n")
    ("c" "codeword" entry (file "codeword.org") "* %? %^G\n")

    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(assoc "d" org-capture-templates)
;(setq org-capture-templates (alist-put org-capture-templates "q" '("blah")))

;(assoc "d" org-capture-templates)
;(assoc "q" org-capture-templates)
;(org-capture-template-add "q" )
;(org-capture-template-remove "q")
;(org-capture-template-add "q" '("org" entry (file+olp "refile.org" "org") "* %? %T %^{fart}\n"))
;(org-capture-template-add "q" '("org" entry (file+olp "refile.org" "org") "* %a %^{fart}p\n"))
;(org-capture-template-add "q" '("org" entry (file+olp "refile.org" "org") "* %A %? %^{fart}p\n"))
;(org-capture-template-add "q" '("org" entry (file+olp "refile.org" "org") "* %?\n%A\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from e:/.p/stub/.emacs.el
(org-capture-template-add "b"
  '("bugs" entry (file "") "* %? :filey:"))


(org-capture-template-add "y"
 `("Table 1" table-line (file+olp "e:/ca/.org/sandbox.org" "tables" "1") "| %?  | | |"  :table-line-pos "II-1"))

(org-capture-template-add "ma"
 `("Table z" table-line (file+olp "e:/ca/.org/sandbox.org" "tables" "1") "| %?  | | |\n"  :table-line-pos "III-2"))

(org-capture-template-add "mb"
 `("Table z" table-line (file+olp "e:/ca/.org/sandbox.org" "tables" "1") "| %?  | | |"  :table-line-pos "-1"))

(org-capture-template-add "m" `("Prefix"))


;; %^C - Interactive selection of which kill or clip to use.

;;; (setq org-table-automatic-realign nil)
