;;; TODO
;;; . save timestamped kill-snippet definitions (autosave on timer)
;;; . sanity check on active snippets
;;;
;;; After a boo insertion, the kill will be what was marked
;;; and the region is not active, even though mark and point are set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar kill-snippet-mode-alist nil
  "Kill snippets organised by major-mode")

;(setq kill-snippet-mode-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq kill-snippet-prefix "ks")

(defun kill-snippet-make-name (tag)
  (format "%s%s"
	  kill-snippet-prefix
	  (cond ((stringp tag) tag) ((char-to-string tag)))
	  )
  )

; (kill-snippet-make-name ?a)
; (kill-snippet-make-name "b")

(defun kill-snippet-last-tag (&optional mode)
  (let* ((mode (or mode major-mode))
	 (ms (alist-get mode kill-snippet-mode-alist))
	 (sort (sort (mapcar 'car ms) 'string>)) 
	 )
    (string-to-char (substring (car sort) (length kill-snippet-prefix)))
    )
  )

(defun kill-snippet-last-name (&optional mode)
  (kill-snippet-make-name (kill-snippet-last-tag mode))
  )

(defun kill-snippet-next-tag (&optional mode)
  (let* ((last (kill-snippet-last-tag mode))
	 )
    (1+ last)
    )
  )

(defun kill-snippet-next-name (&optional mode)
  (kill-snippet-make-name (kill-snippet-next-tag mode))
  )

; (kill-snippet-next-name 'perl-mode)
; (kill-snippet-last-name 'perl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-snippet-delete (name &optional mode)
  (let* ((mode (or mode major-mode))
	 (ms (alist-get mode kill-snippet-mode-alist))
	 )
    (setq kill-snippet-mode-alist
      (alist-put kill-snippet-mode-alist mode 
		 (alist-remprop ms name)))
    )
  )

(kill-snippet-delete "ksc" 'perl-mode)
(kill-snippet-delete "ksb" 'perl-mode)
(kill-snippet-delete "ksd" 'perl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-snippet-compare (a b) (equal (cadr a) (cadr b)))

(defun kill-snippet-sanity-fail (s)
  (cond
   ((> (length s) 200) (format "too long (%s)" (length s)))
   )
  )

(defun kill-snippet-sanity-confirm (s)
  (let ((insane (kill-snippet-sanity-fail s)))
    (and
     insane
     (not (y-or-n-p (format "Snippet text sanity problem - %s. Confirm y/n: " insane)))
     (error "kill-snippet cancelled")
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-snippet-add (s &optional mode name)
  "Add a kill-snippet S to the mode list for MODE.
The NAME will default to \"ks<x>\" where <x> is a,b,...
is the auto generated character.
If NAME is provided this will affect autogeneration of names going forward,
so beware."
  (kill-snippet-sanity-confirm s)
  (let* ((mode (or mode major-mode))
	 (ms (alist-get mode kill-snippet-mode-alist))
	 (tag (kill-snippet-next-tag mode))
	 (name (or name (kill-snippet-make-name tag)))
	 (new (list name s nil nil nil nil nil (format "s-k s-%c" tag)))
	 )

    (setq ms (alist-remprop ms name))
    (setq ms (cons new ms))

    (setq kill-snippet-mode-alist
      (alist-put kill-snippet-mode-alist mode ms)
      )
    (funcall 'yas-define-snippets mode ms)
    (message "new snippet name is '%s'" name)
    )
  )

(defun kill-snippet-remove (&optional mode name)
  "Remove kill-snippet NAME from mode list for MODE.
The NAME will default to the last defined.
MODE defaults to current major-mode."
  (let* ((mode (or mode major-mode))
	 (name (or name (kill-snippet-last-name mode)))
	 )
    (kill-snippet-delete name mode)
    )
  )

(defun kill-snippet-active ()
  "The current candidate for a new kill-snippet.
If the region is active then the new snippet is the contents of the region,
if no region is active, then the new snippet is the head of the kill-ring.

We should add code that will do a sanity-check and a confirmation query
when the candidate does not look like a good snippet."

  (interactive)
  (let (s)
    (cond ((region-active-p)
	   (copy-region-as-kill nil nil t))
	  )
    (setq s (substring-no-properties (car kill-ring)))
    s)
  )
    
(defun kill-snippet-add-active ()
  "Add the current active candidate to the current major-mode kill-snippet list."
  (interactive)
  (kill-snippet-add (kill-snippet-active))
  )

(defun kill-snippet-replace-active ()
  "Replace the last defined kill-snippet with the current active candidate."
  (interactive)
  (kill-snippet-remove)
  (kill-snippet-add-active)
  )

(defun kill-snippet-define (&optional mode)
  "Define yasnippets for the kill-snippet list for MODE."
  (interactive "SMode: ")
  (let* ((mode (or mode major-mode))
	 (list (alist-get mode kill-snippet-mode-alist))
	 )
    (funcall 'yas-define-snippets mode list)
    (message "kill-snippet-define: %s" mode)
    )
  )

(defun kill-snippet-define-all ()
  "Define yasnippets for the kill-snippet list for all modes."
  (interactive)
  (mapcar 'kill-snippet-define (mapcar 'car kill-snippet-mode-alist))
  )

(defun kill-snippet-active-expand ()
  "Add the current active candidate to the current major-mode kill-snippet list,
and immediately expand it.
When an instant expansion required for possible one off snippet."
  (interactive)
  (let ((s (kill-snippet-active)))
    (kill-snippet-add s)
    (yas-expand-snippet s)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; (KEY TEMPLATE 
;;;   (2) NAME CONDITION GROUP 
;;;   (5) EXPAND-ENV LOAD-FILE
;;;   (7) KEYBINDING
;;;   (8) UUID SAVE-FILE)

(defun kill-snippet-edit ()
  "Edit the kill-snippets."
  (interactive)
  (let* ((buf (get-buffer-create "*kill-snip*")))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert ";;; -*- mode: emacs-lisp -*-
;;;
;;; f9		- eval this buffer
;;; s-y s-x	- define all snippets for corresponding modes

")

    (insert "(setq kill-snippet-mode-alist\n `(\n")
    (dolist (i kill-snippet-mode-alist)
      (insert (format "   (%s\n" (car i)))
      (dolist (ii (cdr i))
	(insert (format "    %S\n" ii))
	)
      (insert "   )\n")
      )
    (insert "  )\n")
    )
  (insert " )\n")
  (emacs-lisp-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(global-unset-key (kbd "s-y"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq kill-snippet-map (make-sparse-keymap))

(def-key global-map (kbd "s-y") kill-snippet-map)

(def-key kill-snippet-map (kbd "s-y") 'kill-snippet-active-expand)
(def-key kill-snippet-map (kbd "s-e") 'kill-snippet-edit)
(def-key kill-snippet-map (kbd "s-d") 'kill-snippet-define)
(def-key kill-snippet-map (kbd "s-a") 'kill-snippet-add-active)
(def-key kill-snippet-map (kbd "s-r") 'kill-snippet-replace-active)
(def-key kill-snippet-map (kbd "s-x") 'kill-snippet-define-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(describe-variable 'yas-indent-line)
