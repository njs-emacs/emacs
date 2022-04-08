(mbm-cycle nil '("A" "BB" "CCC"))

`((:match t) nil 
 (j . "A")
 )

()

(mbm-cycle-plus nil nil
    `(
      (b . ".mbm.el")
      (a . "mbm.el")
      (m . "a.mug")
      (p . "<%B>.mug")
      )
    )

(mbm-cycle-plus `(:match t) nil
    `(
      (x . "e:/.org/notes.org")
      (y . "e:/daily/foo.xx")
      (z . "e:/emacs/recent/killsnippet.el")
      )
    )

`((:match t) nil
   (x . "e:/.org/notes.org")
   (y . "e:/daily/foo.xx")
   (z . "e:/emacs/recent/killsnippet.el")
  )

`((:match t) nil
  (b . ".mbm.el")
  (a . "mbm.el")
  (m . "a.mug")
  )



(display-buffer (find-file-noselect "mbm.el") zz-alist)
(display-buffer (find-file-noselect "a.mug") zz-alist)
(display-buffer (find-file-noselect "asn.el") zz-alist)
(display-buffer-pop-up-window (find-file-noselect "a.mug") nil)
(display-buffer-pop-up-window (find-file-noselect "asn.el") nil)
(display-buffer-below-selected (find-file-noselect "a.mug") `((window-min-height . 10) (window-height . 15)))
(apropos "action.*alist")
display-buffer-alist
display-buffer-overriding-action
