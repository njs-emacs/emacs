(load-standard "ediff")

;(add-hook 'ediff-quit-hooks 'ediff-quit-ns t)

;(defun ediff-quit-ns ()
;  (save-excursion (set-buffer ediff-A-buffer)
;		  (save-kill))
;  (save-excursion (set-buffer ediff-B-buffer)
;		  (save-kill))
;  )

(defun venn (a b)
  (list
   (intersection a b)
   (set-difference a b)
   (set-difference b a)
   ))

(defun diff-cmp (x a b)
  (> (length
      (call-shell
       (format "cmp %s/%s %s/%s" a x b x))) 0))

(defun ediff-re (x a b)
  (let* ((a (expand-file-name (format "%s/%s" a x)))
	 (b (expand-file-name (format "%s/%s" b x))))
    (cond ((or (file-directory-p a) (file-directory-p b)))
	  ((ediff-files a b) (re))
	  ))
  )

(defun diff-dir (fun a b &optional as bs)
  (let*
    ((af (mapcar 'file-name-nondirectory (ls (concat "-t " a "/" (or as "")))))
     (bf (mapcar 'file-name-nondirectory (ls (concat "-t " b "/" (or bs "")))))
	 (v (venn af bf))
	 (x (subset-if
	     '(lambda (x)
		(message "comparing %s..." x)
		(let ((z (diff-cmp x a b)))
		  (and z fun (funcall fun x a b))
		  (message "comparing %s... %s" x (if z "differ" "same"))
		  z))
		 (car v)))
	 )
    (apply 'list x (set-difference (car v) x) (cdr v))
    )
  )

(defun ediff-recompute-diffs ()
  "Recompute difference regions in buffers A and B."
  (interactive)
  (let ((point-A (ediff-eval-in-buffer ediff-A-buffer (point)))
	(point-B (ediff-eval-in-buffer ediff-B-buffer (point)))
	file-A file-B)
    (ediff-unselect-and-select-difference -1)
    (ediff-eval-in-buffer
     ediff-A-buffer
     (setq file-A (ediff-make-temp-file)))
    (ediff-eval-in-buffer
     ediff-B-buffer
     (setq file-B (ediff-make-temp-file)))
    (ediff-clear-diff-vector ediff-difference-vector 'fine-diffs-also)
    (setq ediff-killed-diffs-alist nil) ; saved kills will no longer be valid
    	    	    	    	    	; after recompute
    (setq ediff-difference-vector
	  (ediff-setup-diff-regions file-A file-B 'use-old))
    (setq ediff-number-of-differences (length ediff-difference-vector))
    (delete-file file-A)
    (delete-file file-B)
    (ediff-eval-in-buffer ediff-A-buffer (goto-char point-A))
    (ediff-jump-to-difference 1)
    ))

(defun -ediff-next (event) (interactive "e")
  (ediff-next-difference 1)
  )
  
(add-hook 'ediff-startup-hooks
	  '(lambda ()
	     (define-key ediff-mode-map [mouse-1] '-ediff-next)))

(defun files-identical (x)
  (zerop (call-process "sh" nil nil nil "-c"
		       (apply 'format "diff --brief %s %s %s" 
			      diff-args x)))
  )

(defun ediff-file-list-filter-diff (list)
  (let ((z (delete-if 'files-identical (cdr list))))
    (cons (car list) z)
    ))

(defvar ediff-file-list-filter 'ediff-file-list-filter-diff
  "")
(defvar diff-args "-B"
  "")

(defun ns-ediff-load-hook ()
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (cond
   ((ediff-device-type)
    (ediff-toggle-multiframe)
    )
   )
  )

(add-hook 'ediff-load-hook 'ns-ediff-load-hook)

;(add-hook 'ediff-cleanup-hook 'ediff-janitor)
;(setq ediff-cleanup-hook nil)
;(set-default 'ediff-diff-options "-w")

;(require 'vc)
;
;(define-key vc-prefix-map "e" 'ediff-revision)

(load-overrides "ediff")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edb (a b) (interactive 
   (let (bf)
     (list (setq bf (ediff-other-buffer ""))
	   (read-buffer "Buffer B to compare: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (ediff-buffers a b)
  )

;#~ no ediff-map as this would imply that ediff owns prefix key

(global-set-key "\M-e\M-e" 'edb)
(global-set-key "\M-e\M-b" 'ediff-buffers)
(global-set-key "\M-e\M-f" 'ediff-files)
(global-set-key "\M-e\M-d" 'ediff-directories)

(defun ediff-base-with-variants (a b &rest list)
  (dolist (i list)
     (ediff-files (format "%s%s" b a) (format "%s%s" i a))
     (re)
     )
  )

(fset 'edbv 'ediff-base-with-variants)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ediff-kill () (interactive)
  (let* ((buf-a (make-buffer "*edk-a*"))
	 (buf-b (make-buffer "*edk-b*"))
	 )
    (set-buffer buf-a)
    (erase-buffer)
    (insert (nth 0 kill-ring))
    (set-buffer buf-b)
    (erase-buffer)
    (insert (nth 1 kill-ring))
    (ediff-buffers buf-a buf-b)
    )
  )


(global-set-key "\M-e\M-k" 'ediff-kill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ediff-revision-1 "")
(setq ediff-revision-2 "")

(defun ediff-revision-latest () (interactive)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
	 (rev1 ediff-revision-1)
	 (rev2 ediff-revision-2)
	 startup-hooks)
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     rev1 rev2 startup-hooks)
    )
)
(global-set-key "\M-e\M-l" 'ediff-revision-latest)
(global-set-key "\M-e\M-v" 'ediff-revision)

(defun ediff-revision-stash () (interactive)
  "Ediff the current buffer with the most recently stashed version"
  (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     "stash" "" nil)
  )

(define-key vc-prefix-map "z" 'ediff-revision-stash)
(define-key vc-prefix-map "e" 'ediff-revision-latest)

; must run a vanilla ediff-revision before vc-ediff-stash
