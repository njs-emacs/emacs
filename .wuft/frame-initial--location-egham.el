;; this is called after the default-frame-alist is cloned to initial-frame-alist
;; so changes to default-frame-alist must be also made to initial-frame-alist
;; if this is too much aggro put it in frame-default.el

(setq initial-frame-alist
  (alist-merge initial-frame-alist
	       `(
		 (left . 160)
		 )
	       )
  )
