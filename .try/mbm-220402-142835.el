(mbm-include "e:/home/nick/.mbm.el")

(mbm-cycle `(foo 7) '("A" "B" "C"))
;(mbm-cycle `(foo 7) '(".mbm.el" "foo.xx"))

(mbm-cycle-plus nil nil
    `(
      (o . "<%B>.org")
      (e . "<%B>.el")
      (c . "<%B>.c")
      (y . "<%B>.y")
      (x . "<%B>.h")
      )
    )

`((:match t) nil (a . ".mbm.el") (b . "."))

()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mbm--name-equal "foo" "foo")
(mbm--name-equal "foo.c" "\\.c$")
(mbm--name-equal "foo.c" "\\.c$" t)
(mbm-file-match "foo.c" '("\\.c$") t)
(mbm-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq zz
  
  )
(mbm-expansion-to-regexp "<%B>.z")


(mbm-eval zz "zz.c" 'next)
(mbm-eval zz "zz.y" 'next)
(mbm-eval zz "zz.h" 'next)
(mbm-eval zz "zz.c" 'prev)
(mbm-eval zz "zz.y" 'prev)
(mbm-eval zz "zz.h" 'prev)

(mbm-eval zz "zz.c" 'y)
(mbm-eval zz "ii.y" 'c)

(mbm-eval (mbm-cycle `(foo 7) '("A" "B" "C")) "Z" 'next)
(mbm-eval (mbm-cycle `(foo 7) '("A" "B" "C")) "A" 'next)
(mbm-eval (mbm-cycle `(foo 7) '("A" "B" "C")) "B" 'next)
(mbm-eval (mbm-cycle `(foo 7) '("A" "B" "C")) "C" 'next)
(mbm-eval (mbm-cycle `(foo 7) '("A" "B" "C")) "A" 'prev)
(mbm-eval (mbm-cycle `(foo 7) '("A" "B" "C")) "B" 'prev)
(mbm-eval (mbm-cycle `(foo 7) '("A" "B" "C")) "C" 'prev)

(mbm-eval `(nil ("A" "B" "C") (kool . "Z") (plop . "Q")) "B" 'kool)

; with template expansion
; inclusion is filename set

(mbm-eval `(nil ("A" "B" "C") (kool . "<%B>.c") (plop . "<%B>.y")) "A" 'kool)
(mbm-eval `(nil ("A" "B" "C") (kool . "<%B>.c") (plop . "<%B>.y")) "A" 'plop)

; inclusion is single filename

(mbm-eval `(nil "A" (kool . "<%B>.c") (plop . "<%B>.y")) "A" 'plop)
(mbm-eval `(nil "A" (kool . "<%B>.c") (plop . "<%B>.y")) "A" 'kool)

; inclusion is single regexp

(mbm-eval `((:regexp t) "\\.c$" (kool . "<%B>.c") (plop . "<%B>.y")) "A.c" 'plop)
(mbm-eval `((:regexp t) "\\.c$" (kool . "<%B>.c") (plop . "<%B>.y")) "zxxx.c" 'plop)
(mbm-eval `((:regexp t) "\\.c$" (kool . "<%B>.k") (plop . "<%B>.y")) "zxxx.c" 'kool)

; inclusion is regexp list

(mbm-eval `((:regexp t) ("\\.c$" "\\.h") (kool . "<%B>.c") (plop . "<%B>.y")) "A.c" 'plop)
(mbm-eval `((:regexp t) ("\\.c$" "\\.h") (kool . "<%B>.c") (plop . "<%B>.y")) "A.h" 'plop)
(mbm-eval `((:regexp t) ("\\.c$" "\\.h") (kool . "<%B>.c") (plop . "<%B>.y")) "A.z" 'plop)

; no inclusion parameter, built from targets
(mbm-eval `(nil nil (kool . "Z") (plop . "Q")) "B" 'kool) ; not found
(mbm-eval `(nil nil (kool . "Z") (plop . "Q")) "Q" 'kool)

; you can't use regexps on targets as they are replacement templates


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mbm-eval `(nil nil (kool . "Z") (plop . "Q") (qwez . "B")) "B" 'kool)

(string-match "Z" "Q")

(replace-regexp-in-string  "Z" "Q" "ZZZ")

(replace-regexp-in-string "<\\(.*?\\)>" 'fooz "<A><B><C>" nil nil 1)
(replace-regexp-in-string "<\\([A-Z]*\\)>" 'fooz "<A><B><C>" nil nil 0)
(replace-regexp-in-string "<\\([A-Z]\\)>" 'fooz "<A><B><C>" nil nil 1)
(replace-regexp-in-string "<\\([A-Z]*\\)>" 'fooz "<A><B><C>" nil nil 1)
(replace-regexp-in-string "<\\([A-Z]*\\)>" 'fooz "<ABC>" nil nil 1)

; the bit outside the substring is not replaced but the whole match is


(mbm-cycle () `("a" "b"))

(mbm-list () "^[0-9]"
  (a . "02")
  (b . "03")
  (c . "04")
  )

;;; might like a dual purpose form that has random access using keys
;;; but also next/prev/first/last by finding current position in list
;;; should add a plist parameter at the start and deprecate using
;;; raw forms as above. They're not much easier to input, and are
;;; vulnerable to internal changes
;;; It would be best to replace .mq with a new framework mapped to
;;; the same key scheme.
;;;
;;; mbm - meta buffer mapping
;;; .mbm.el is our control file
;;;

()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mbm-list () nil
 "A"
 "B"
 "C"
 )

if the match-predicate-arg is nil, then the match set is the link set

(mbm-list () '("A" "B" "C"))

if the match-predicate-arg is a list, and the link set is null,
then the link set is the same as the predicate arg

(mbm-list ()
 '("A" "B" "C")
 "A"
 "B"
 "C"
)

