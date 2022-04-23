;(dregf "hydra" elfs "year")
;(dregf "read-from-minibuffer" elfs "year")

(defun org-relevance-line-get-or-add (s &optional line)
  (bob)
  (cond
   ((rsf "^#\\+RELEVANCE: " 10000)
    (kill-line nil)
    (insert s)
    )
   (t (and line (goto-line line))
      (insert "#+RELEVANCE: " s "\n")
      (fl -1)
      (eol)
      )
   )
  )

(defun org-relevance-set (new)
  (sx
   (org-relevance-line-get-or-add new 2)
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-u-property-get (tag)
  (bob)
  (cond
   ((rsf (format "^#\\+%s: " tag) 10000)
    (bs (point) (point$))
    )
   )
  )

(defun org-u-property-replace (tag s &optional line)
  (bob)
  (cond
   ((stringp line)
    (setq line (or (sx (bob) (and (rsf line nil t) (1+ (line-number-at-pos)))) 0)))
   )
  (cond
   ((rsf (format "^#\\+%s: " tag) 10000)
    (kill-line nil)
    (insert s)
    )
   (t (and line (goto-line line))
      (insert (format "#+%s: " tag) s "\n")
      (fl -1)
      (eol)
      )
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-u-tag-read (tag &optional prompt history)
  (let* ((dtag (downcase tag))
	 (prompt (or prompt (format "%s: " (capitalize dtag))))
	 (history (or history (intern (format "org-%s-history" dtag))))
	 )
    (list
     (eval
      `(read-from-minibuffer
	prompt
	(or (org-u-property-get tag)
	    (car ,history))
	nil nil history))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-topic-replace (new)
  (interactive
    (list
     (read-from-minibuffer
      "Topics: "
      (or (org-u-property-get "TOPIC")
	  (car org-topic-history))
      nil nil 'org-topic-history))
    )
  (org-u-property-replace "TOPIC" new 2)
  )

(defun org-topic-replace (new)
  (interactive (org-u-tag-read "TOPIC"))
  (org-u-property-replace "TOPIC" new "^#\\+\\(CLASS\\|TOPIC\\)")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-description-history nil)

(defun org-description-replace (new)
  (interactive (org-u-tag-read "DESCRIPTION"))
  (org-u-property-replace "DESCRIPTION" new "^#\\+\\(CLASS\\|TOPIC\\)")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-class-line-get-or-add ()
  (bob)
  (cond
   ((rsf "^#\\+CLASS: " 10000))
   (t (insert "#+CLASS: \n") (fl -1) (eol))
   )
  )

(defun org-class-add-toggle (new &rest stuff)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-class-history nil)
(setq org-topic-history nil)

(defun org-class-insert (class)
  (interactive
    (list (read-from-minibuffer "Class: " (car org-class-history) nil nil 'org-class-history))
    )
  (org-class-add-toggle class)
  )

(setq org-class-hydra--base-map (make-sparse-keymap))
(defhydra org-class-hydra (:hint nil :color pink :base-map org-class-hydra--base-map)
      "
_g_:   General     _h_:   Help        _n_:   Notes          _1_: trash
_t_:   Todo        _l_:   Log         _S_:   Spreadsheet    _2_: blah
_d_:   Document    _i_:   Index       _k_:   Keymap         _3_: alright
_a_:   Archive     _r_:   Refile                          _4_: important
_c_:   Copy        _e_:   Emptied     _j_:   Junk           _5_: crucial
_s_:   Sandbox     _p_:   Project     _b_:   Buglist  
_o_:   Other       _T_:   Topics   	
_q_:   Quit
_!_:   Shell
"
  ("a" (org-class-add-toggle "archive" "stuff archived from other locations"))
  ("b" (org-class-add-toggle "buglist"))
  ("c" (org-class-add-toggle "copy" "stuff which exists somewhere else"))
  ("d" (org-class-add-toggle "document"))
  ("e" (org-class-add-toggle "emptied" "stub remaining after deletion or refile"))
  ("g" (org-class-add-toggle "general"))
  ("G" (org-class-add-toggle "global"))
  ("h" (org-class-add-toggle "help"))
  ("i" (org-class-add-toggle "index"))
  ("j" (org-class-add-toggle "junk"))
  ("k" (org-class-add-toggle "keymap"))
  ("l" (org-class-add-toggle "log"))
  ("m" (org-class-add-toggle "manual"))
  ("n" (org-class-add-toggle "notes"))
  ("p" (org-class-add-toggle "project"))
  ("r" (org-class-add-toggle "refile"))
  ("s" (org-class-add-toggle "sandbox"))
  ("S" (org-class-add-toggle "spreadsheet"))
  ("t" (org-class-add-toggle "todo"))

  ("D" (call-interactively 'org-description-replace))
  ("T" (call-interactively 'org-topic-replace))
  ("o" (call-interactively 'org-class-insert))

  ("1" (org-relevance-set "TRASH"))
  ("2" (org-relevance-set "BLAH"))
  ("3" (org-relevance-set "ALRIGHT"))
  ("4" (org-relevance-set "IMPORTANT"))
  ("5" (org-relevance-set "CRUCIAL"))

  ("!" (call-interactively 'shell-command))

  ("q" nil :color blue)
  )

(define-key global-map (kbd "s-1 s-1") 'org-class-insert)
(define-key global-map (kbd "s-1 s-2") 'org-class-hydra/body)

