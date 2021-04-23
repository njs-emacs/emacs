(defun yas-x-apply (fun n)
  (let* ((f (yas--snippet-find-field snippet n))
	 )
    (funcall fun f)
    )
  )

(defun yas-x-field-modified-p (n) (yas-x-apply 'yas--field-modified-p n))

(top-level)

(setq yas-indent-line nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun crud ()
;  (if (yas-x-field-modified-p 1) "yes" "no"))
  (format "%s %s" (yas-x-field-modified-p 1) (yas-x-field-modified-p 2))
  )


${1:"Title"}
label ${2:"waiting for call..."$(if yas-moving-away-p (crud))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; nested fields allow fields to be skipped (in this case the id value)
; when the containing field (an id property field) is skipped (C-d)
<div${1: id="${2:some_id}"}>$0</div>

<div id="some_id"></div>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
${1:"Title"}
label ${2:"waiting for call..."$(if yas-modified-p "yes" "no")}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this will keep the contents of field 2 if it is modified
; but replace with field 1 if not
; this is not unique or useful behaviour, but illustrates access
; to properties of other fields which we did not know how to gain
;
${1:"Title"}
label ${2:"waiting for call..."$(if yas-moving-away-p
				    (if (yas-x-field-modified-p 2)
					nil
				      (yas-field-value 1)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this is a transformation in field, not a mirror because there is something
; (e.g text, or just $ if you don't want it) which causes the transformation
; to be applied to this field

#define "${1:m$(upcase yas-text)}"
#define "${1:$$(upcase yas-text)}"


>>>
#define "MYDEFINEASNXSJDJDJDJSDS"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; yas-field-value is a standard exported property accessor
; we have written a non-standard one yas-x-apply to access some
; other properties

(format "${1:formatted %s}" "${2:value}")
=> "${1:$(ignore-errors (format (yas-field-value 1) (yas-field-value 2)))}"

;All mirrors are updated when any field is updated


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
backticks are for initial snippet content

for ($1;$2;$3) {
  `yas-selected-text`$0
}

;; this only works if you have this snippet mapped to a key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yas-wrap-around-region places the selection where $0 was

(setq yas-wrap-around-region t)

for ($1;$2;$3) {
  $0
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; snippets can be used to do a live translation/encode/decode of
; typed text
;
(defun fsdump (s fmt)
  (apply 'concat
	 (mapcar '(lambda (x) (format fmt x))
		      (string-to-list s))))

keys:  $1
char:  ${1:$(fsdump yas-text "  %-2c")}
dec:   ${1:$(fsdump yas-text "%3d ")}
hex:   ${1:$(fsdump yas-text " %02x ")}


