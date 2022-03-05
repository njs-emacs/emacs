;;; TODO
;;; . save timestamped kill-snippet definitions (autosave on timer)
;;; . sanity check on active snippets
;;;
;;; After a boo insertion, the kill will be what was marked
;;; and the region is not active, even though mark and point are set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mode* (&optional mode) (or mode major-mode))

(defun mode-put (prop value &optional mode)
  (put (mode* mode) prop value)
  )

(defun mode-get (prop &optional mode)
  (get (mode* mode) prop)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar kill-snippet-mode-alist nil
  "Kill snippets organised by major-mode")

;(setq kill-snippet-mode-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-snippet-mru (&optional mode)
  (or
   (mode-get 'kill-snippet-mru mode)
   (error "No most recent kill-snippet in %s" (mode*))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq kill-snippet-prefix "ks")

(defun kill-snippet-binding (tag)
;  (format "s-k s-%c" tag)
  (format "H-k H-%s" tag)
  )

(defun kill-snippet-make-name (tag)
  (format "%s%s"
	  kill-snippet-prefix
	  (cond ((stringp tag) tag) ((char-to-string tag)))
	  )
  )

(defun kill-snippet-last-name (&optional mode)
  (let ((tag (kill-snippet-mru mode)))
    (kill-snippet-make-name tag)
    )
  )

(defun kill-snippet-highest-busy-tag (&optional mode)
  (let* ((mode (or mode major-mode))
	 (ms (alist-get mode kill-snippet-mode-alist))
	 (sort (sort (mapcar 'car ms) 'string>)) 
	 )
    (cond
     (sort (string-to-char (substring (car sort) (length kill-snippet-prefix))))
     )
    )
  )

(defun kill-snippet-lowest-free-tag (&optional mode)
  (let* ((last (kill-snippet-highest-busy-tag mode))
	 )
    (cond (last (1+ last)) (?a))
    )
  )

(defun kill-snippet-next-name (&optional mode)
  (kill-snippet-make-name (kill-snippet-lowest-free-tag mode))
  )

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
(defun kill-snippet-add (s &optional tag mode)
  "Add a kill-snippet S to the mode list for MODE.
TAG defaults to one higher than highest tag named so far."
  (kill-snippet-sanity-confirm s)
  (let* ((mode (or mode major-mode))
	 (ms (alist-get mode kill-snippet-mode-alist))
	 (tag (or tag (kill-snippet-lowest-free-tag mode)))
	 (name (kill-snippet-make-name tag))
	 (new (list name s nil nil nil nil nil (kill-snippet-binding tag)))
	 )

    (setq ms (alist-remprop ms name))
    (setq ms (cons new ms))

    (setq kill-snippet-mode-alist
      (alist-put kill-snippet-mode-alist mode ms)
      )
    (funcall 'yas-define-snippets mode ms)
    (mode-put 'kill-snippet-mru tag mode)
    (message "new snippet name is '%s'" name)
    )
  )

(defun kill-snippet-remove (&optional tag mode)
  "Remove kill-snippet identified by TAG from mode list for MODE.
The TAG will default to the last defined.
MODE defaults to current major-mode."
  (let* ((mode (or mode major-mode))
	 (tag (kill-snippet-mru mode))
	 (name (kill-snippet-make-name tag))
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
    
(defun kill-snippet-add-active (&optional tag)
  "Add the current active candidate to the current major-mode kill-snippet list."
  (interactive 
    (list (read-from-minibuffer "Tag: " (char-to-string (kill-snippet-lowest-free-tag))))
    )
  (cond ((string= tag "") (setq tag nil)))
  (kill-snippet-add (kill-snippet-active) tag)
  )

(defun kill-snippet-replace-active (&optional tag)
  "Replace a kill-snippet corresponding to TAG with the current active candidate.
TAG defaults to most recently defined."
  (interactive 
    (list (read-from-minibuffer "Tag: " (kill-snippet-mru)))
    )
  (let* ((mode major-mode)
	 (ms (alist-get mode kill-snippet-mode-alist))
	 (name (kill-snippet-make-name tag))
	 (x (alist-get name ms nil nil 'equal))
	 )
    (cond (x
	   (setcar x (kill-snippet-active))
	   (funcall 'yas-define-snippets mode ms)
	   )
	  )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; yas-describe-tables - show all active snippets

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
  (insert " )\n\n(kill-snippet-define-all)\n\n")
  (emacs-lisp-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-snippet-set (list &optional merge)
  ; use to optionally merge old kill-snippets with new
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(global-unset-key (kbd "s-y"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq kill-snippet-map (make-sparse-keymap))

(define-key global-map (kbd "s-y") kill-snippet-map)

(def-key kill-snippet-map (kbd "s-y") 'kill-snippet-active-expand)
(def-key kill-snippet-map (kbd "s-e") 'kill-snippet-edit)
(def-key kill-snippet-map (kbd "s-d") 'kill-snippet-define-for-mode)
(def-key kill-snippet-map (kbd "s-a") 'kill-snippet-add-active)
(def-key kill-snippet-map (kbd "s-r") 'kill-snippet-replace-active)
(def-key kill-snippet-map (kbd "s-x") 'kill-snippet-define-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(describe-variable 'yas-indent-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (kill-snippet-delete "ksc" 'perl-mode)
;;; (kill-snippet-delete "ksb" 'perl-mode)
;;; (kill-snippet-delete "ksd" 'perl-mode)
;;; (kill-snippet-next-name 'perl-mode)
;;; (kill-snippet-last-name 'perl-mode)
;;; (kill-snippet-make-name ?a)
;;; (kill-snippet-make-name "b")

;;; (mode-put 'kill-snippet-mru "c" 'perl-mode)
;;; (kill-snippet-last-name 'perl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mark a region or otherwise the current line is used
;; kill-snippet-add-active defines a snippet in the current mode
;; use (yas-describe-tables)
