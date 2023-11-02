;;; dreg.el --- emacs interface to history search (dreg)

;; Copyright (C) 2023 Otzo Software 

;; Author: Nick Steed <nick@otzo.org>
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: emacs, dreg
;; URL: https://flamingant.github.io/dreg

;;; Commentary:

;; dreg is a subsystem used to find patterns in previously edited files.

(defvar dreg-cmd-default "year" "Default dreg command")
(defvar dreg-extra-args "" "Extra dreg command line parameters")

(setq dreg-cmd-default "ever")

;(setq dreg-script-file "e:/_backup/.meta/dreg/dreg.pl")
(setq dreg-script-file "e:/_backup/.meta/dreg14/dreg14.pl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 231012-172543 workaround path problems where c:\\Strawberry\\c\\bin
; must be in path to run dregf.
; An inelegant solution, but establishes a technique for temporary
; PATH override
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dreg** (&rest args)
  "Execute the external dreg command, a perl script."
 (let ((cmd (format "perl %s" dreg-script-file))
       (old-path (getenv "PATH"))
       )
  (setq cmd (mconcat (apply 'list cmd args) " "))
  (unwind-protect
      (let ((new-path (concat old-path ";c:\\Strawberry\\c\\bin")))
	(setenv "PATH" new-path)
	(compile cmd)
	)
    (setenv "PATH" old-path)
    )
  )
 )

(defun dregf (pat &optional fpat cmd &rest args)
  "Perform a dreg query."
 (setq cmd (or cmd dreg-cmd-default))
 (setq args (cons (format "--cmd=%s" cmd) args))
 (cond (fpat (setq args (push (format "--fpat=\"%s\"" fpat) args))))
 (apply 'dreg** (format "--pat=\"%s\"" pat) args)
 )

(defun dref (&optional fpat cmd &rest args)
 (cond (fpat (setq args (push (format "--fpat=\"%s\"" fpat) args))))
 (setq args (push (format "--cmd=%s" (or cmd dreg-cmd-default)) args))
 (apply 'dreg** "--typefilter=." "--pat=\".\"" "--one=1" args)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun drez (pat)
  (interactive "SPattern: ")
  "Perform a special query to just print first line of files which match pattern."
  (let ((tr "and time between now() - interval '1 year' and now() "))
    (dreg**
     (format "--cmd=querz \"select file from log where file ~ '%s' and type = 'S' %s order by time desc limit 100\"" pat tr)
     )
    )
)

(defun drezz (x)
  (interactive "SType: ")
  (drez (format "\\.%s$" x))
  )

(defun dfp (pat &rest args) (apply 'dregf pat plfs args))
(defun dfh (pat &rest args) (apply 'dregf pat htfs args))

(defun dresh (p &optional since)
  (dregf p "sh-.*\\.input" (or since dreg-since "month"))
  )

(defun dregx (&optional arg)
  "Perform dreg query using selection."
  (interactive "p")
  (dregf (x-get-selection) (dired-glob-regexp (grep-spec))
	      (cond
	       ((= arg 4) "ever")
	       ((= arg 1) "month")
	       ((= arg 0) "year")
	       ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq elfs "\\.(el|emacs)$")
(setq eelfs "\\.(eel|el|emacs)$")
(setq htfs "\\.(html|css|php|js)$")
(setq plfs "\\.p[lm]$")
(setq cfs "\\.[ch]$")
(setq nlfs "\\.nl$")
(setq ofs "\\.(el|org)$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yas-x)

(defvar dreg-mode-keymap (make-sparse-keymap) "dreg-mode keymap.")
(setq dreg-arg-map (make-sparse-keymap))

(define-minor-mode dreg-mode
  "."
  :init-value nil
  :lighter " dreg "
  :keymap dreg-mode-keymap
  (cond 
   (dreg-mode
    (yas-x-define (kbd "C-c C-x") "dr")
    )
   )
  )

(defun dreg-map (key arg)
  (let ((key (read-kbd-macro (format "C-c C-%s" key))))
    (define-key dreg-arg-map key arg)
    (define-key dreg-mode-keymap key 'dreg-insert)
    )
  )

(defun dreg-insert (&optional when) (interactive "p")
  (let* ((keys (this-command-keys-vector))
	 (binding (lookup-key dreg-arg-map keys))
	 )
    (insert "\n(dregf \"")
    (sx
     (insert (format "\" %s \"%s\")" binding
		    (cond
		     ((= when 4) "year")
		     ((= when 1) "ever")
		     (t "month")
		     )
		    )
	     ))
    )
  )

(dreg-map "e" "elfs")
(dreg-map "h" "htfs")
(dreg-map "c" "cfs")
(dreg-map "p" "plfs")
(dreg-map "n" "\"\\\\.nl\"")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dreg-shuffle () (interactive)
  (let* ((where (sexp-where))
	 )
    (cond
     ((= where 3)
      (sx (replace-thing-at-point-fun 'sexp 'dreg-shuffle-replace-3))
      )
     )
    )
  )

(defun dreg-shuffle-replace-3 (x)
  (let ((foo (read (read x))))
    (case foo
      (ever "\"month\"")
      (month "\"year\"")
      (year "\"ever\"")
      )
    )
  )


(define-key dreg-mode-keymap (kbd "M-#") 'dreg-shuffle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dreg-guess-best-filespec ()
  (cond
   ((eq major-mode 'emacs-lisp-mode) elfs)
   ((eq major-mode 'nl-mode) "\\.nl$")
   ((eq major-mode 'c-mode) cfs)
   ((concat "\\" (file-name-suffix)))
   )
  )

(defun dreg-dwim (pat where when)
  (interactive
    (list
     (read-from-minibuffer "Pattern: " (region-or-thing))
     (read-from-minibuffer "Where: " (dreg-guess-best-filespec))
     (completing-read "When: " '("ever" "year" "month" "week") nil t nil nil "ever")
     )
    )
  (dregf pat where (read when))
  )

(def-key global-map (kbd "H-g H-g") 'dreg-dwim)
(def-key global-map (kps "78") 'dreg-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (dreg** "--pat=1 week" "--where=file~'.p[lm]'" "--cmd=since" "--when='01 jan 2010'")

; (dregf "time" ".p[lm]" "week")
; (dregf "time" ".p[lm]" "today")
; (dregf "time" ".p[lm]")

