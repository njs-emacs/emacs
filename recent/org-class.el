;(dregf "hydra" elfs "year")
;(dregf "read-from-minibuffer" elfs "year")

(defun org-class-line-get-or-add ()
  (bob)
  (cond
   ((rsf "^#\\+CLASS: " 10000))
   (t (insert "#+CLASS: \n") (fl -1) (eol))
   )
  )

(defun org-class-add-toggle (new)
  (sx
   (let* ((new (intern new))
	  (classes (org-class-line-get-or-add)))
     (setq list (read (format "(%s)" (bs (point) (point$)))))
;;;    (debug)
     (cond
      ((memq new list) (setq list (delq new list)))
      ((setq list (append list (list new))))
      )
     (delete-region (point) (point$))
     (insert (mconcat (mapcar 'symbol-name list) " "))
     )
   )
  )

(setq org-class-history nil)

(defun org-class-insert (class)
  (interactive
    (list (read-from-minibuffer "Class: " (car org-class-history) nil nil 'org-class-history))
    )
  (org-class-add-toggle class)
  )

(setq org-class-hydra--base-map (make-sparse-keymap))
(defhydra org-class-hydra (:hint nil :color pink :base-map org-class-hydra--base-map)
      "
_g_:   General	_t_:   Todo	_l_:   Log	
_h_:   Help	_s_:   Sandbox	_d_:   Document
_j_:   Junk	_i_:   Index	_b_:   Buglist
_o_:   Other    _n_:   Notes	_r_:   Refile
_a_:   Archive
_q_:   Quit
	
"
  ("a" (org-class-add-toggle "archive"))
  ("b" (org-class-add-toggle "buglist"))
  ("d" (org-class-add-toggle "document"))
  ("g" (org-class-add-toggle "general"))
  ("h" (org-class-add-toggle "help"))
  ("i" (org-class-add-toggle "index"))
  ("j" (org-class-add-toggle "junk"))
  ("l" (org-class-add-toggle "log"))
  ("n" (org-class-add-toggle "notes"))
  ("r" (org-class-add-toggle "refile"))
  ("s" (org-class-add-toggle "sandbox"))
  ("t" (org-class-add-toggle "todo"))

  ("o" (call-interactively 'org-class-insert))

  ("q" nil :color blue)
  )

(define-key global-map (kbd "s-1 s-1") 'org-class-insert)
(define-key global-map (kbd "s-1 s-2") 'org-class-hydra/body)

