;; there is a bug which causes ediff to fail when the filename contains
;; curly brackets. This seems to be unfixable behaviour of call-process
;; when the brackets are in a particular place, for example
;; "foo{6}" -> "foo6"
;; "bar foo{6}" -> "bar foo{6}"
;; 
;; the simplest workaround is to use bash to invoke diff

;; the following is the work-area used to diagnose the problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ediff-files "e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{6}~")
(ediff-files "e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{10}~")

(ediff-files "e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{6}~")
(ediff-files "e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{9}~")
(find-file "e:/emacs/recent/mug.el.~@{9}~")
(ediff-files "e:/emacs/recent/mug.el.~@{6}~" "e:/emacs/recent/mug.el.~@{10}~")


(defun ediff-exec-process (program buffer synch options &rest files)
  "Execute the diff PROGRAM.

The PROGRAM output is sent to BUFFER, which must be a live buffer
object.

The PROGRAM is executed asynchronously unless `system-type' is
`windows-nt' or `ms-dos', or SYNCH is non-nil.

OPTIONS is a string of space-separated options to pass to PROGRAM.  It
may be a blank string.

FILES is a list of filenames to pass to PROGRAM; nil and \"\" elements
are ignored."
;  (debug)
  (let ((data (match-data))
	;; If this is a buffer job, we are diffing temporary files
	;; produced by Emacs with ediff-coding-system-for-write, so
	;; use the same encoding to read the results.
	(coding-system-for-read
	 (if (string-match "buffer" (symbol-name ediff-job-name))
	     ediff-coding-system-for-write
	   ediff-coding-system-for-read))
        (process-environment
         ;; Avoid localization of messages so we can parse the output.
         (cons "LC_MESSAGES=C" process-environment))
        args)
    (setq args (append (split-string options)
                       (mapcar (lambda (file)
                                 (when (stringp file)
                                   (file-name-unquote
                                    (or (file-local-copy file) file))))
                               files)))
    (setq args (delete "" (delq nil args))) ; delete nil and "" from arguments
    ;; the --binary option, if present, should be used only for buffer jobs
    ;; or for refining the differences
    (or (string-match "buffer" (symbol-name ediff-job-name))
	(eq buffer ediff-fine-diff-buffer)
	(setq args (delete "--binary" args)))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          ;; default-directory may be on some remote machine
          ;; (e.g. accessed via Tramp or url-handler) or a non-existing dir.
          (setq default-directory "/")
 	    (debug)
         (if (or (memq system-type '(ms-dos windows-nt))
                  synch)
              ;; In Windows do it synchronously, since Windows doesn't let us
              ;; delete files used by other processes. Thus, in ediff-buffers
              ;; and similar functions, we can't delete temp files because
              ;; they might be used by the asynch process that computes
              ;; custom diffs. So, we have to wait till custom diff
              ;; subprocess is done.
              ;; In DOS, must synchronize because DOS doesn't have
              ;; asynchronous processes.
              (apply #'call-process program nil buffer nil args)
            ;; On other systems, do it asynchronously.
            (let ((proc (get-buffer-process buffer)))
	      (if proc (kill-process proc)))
	    (let ((proc
		   (apply #'start-process "Custom Diff" buffer program args)))
	      (setq mode-line-process '(":%s"))
	      (set-process-sentinel proc #'ediff-process-sentinel)
	      (set-process-filter proc #'ediff-process-filter)
	      )))
      (store-match-data data))))

(apply 'call-process "diff" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "\\{6\\}"))/usr/bin/diff: {6}: No such file or directory

(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "\\{6\\}"))
(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "~@{6}~"))
(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "~@\\{6\\}~"))

(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "mug.el.~@\\{6\\}~"))
(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/mug.el.~@\\{6\\}~"))
(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "/cygdrive/e/mug.el.~@\\{6\\}~"))

(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "/cygdrive/e/emacs/recent/mug.el.~@\\{6\\}~"))e:/emacs/recent/mug.el /cygdrive/e/emacs/recent/mug.el.~@{6}~

(apply 'call-process "diff" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "/cygdrive/e/emacs/recent/mug.el.~@\\{6\\}~"))
(apply 'call-process "diff" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "/cygdrive/e/emacs/recent/mug.el.~@{6}~"))
(apply 'call-process "diff" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@\\{6\\}~"))
(apply 'call-process "diff" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{6}~"))

(apply 'call-process "c:/cygwin64/bin/diff" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{6}~"))/usr/bin/diff: e:/emacs/recent/mug.el.~@6~: No such file or directory


(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/mug.el.~@\\{6\\}~"))
(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@\\{6\\}~"))

(dotimes (i 10) (call-process "diff" nil (current-buffer) nil "e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{6}~"))
(call-process "vi" nil (current-buffer) nil "e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{6}~"):n

(call-process "diff" nil (current-buffer) nil "e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{6}~")


(call-process "cat" nil (current-buffer) nil "e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{6}~")

(call-process "diff" nil (current-buffer) nil "e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@(6)~")

(apply 'call-process "diff" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@\\{6\\}~"))

(apply 'call-process "diff" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{{6}}~"))
(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.~@{6}~"))e:/emacs/recent/mug.el e:/emacs/recent/mug.el.~@6~

(apply 'call-process "echo" nil (current-buffer) nil `("e:/emacs/recent/mug.el" "e:/emacs/recent/mug.el.*"))e:/emacs/recent/mug.el e:/emacs/recent/mug.el.{6} e:/emacs/recent/mug.el.~@{10}~ e:/emacs/recent/mug.el.~@{6}~ e:/emacs/recent/mug.el.~@{9}~ e:/emacs/recent/mug.el.~f8c8dffbf67b437eba8b286153b6f0f8369e9e5a~ e:/emacs/recent/mug.el.~stash{0}~ e:/emacs/recent/mug.el.~{6}~



(call-process "bash" nil (current-buffer) "-c" "cat e:/emacs/recent/mug.el.~@\\{6\\}~")
(call-process "bash" nil (current-buffer) nil "-c" "cat ../recent/mug.el.~@{6}~")
(call-process "bash" nil (current-buffer) nil "-c" "diff ../recent/mug.el ../recent/mug.el.~@{6}~")
(call-process "bash" nil (current-buffer) nil "-c" "echo ../recent/mug.el ../recent/mug.el.~@{6}~")

(call-process "diff" nil (current-buffer) nil "../recent/mug.el" "../recent/mug.el.~@{6}~")
(call-process "cat" nil (current-buffer) nil "../recent/mug.el" "../recent/mug.el.~@{6}~")

(call-process "echo" nil (current-buffer) nil "-c" "../recent/mug.el.~@{6}~")
(call-process "bash" nil (current-buffer) nil "-c" "echo ../recent/mug.el.~@{6}~")

(call-process "echo" nil (current-buffer) nil "-c" "~@{6}~")
(call-process "echo" nil (current-buffer) nil "echo {6}" "{6}")
(call-process "echo" nil (current-buffer) nil "echo{6}" "{6}")
(call-process "bash" nil (current-buffer) nil "-c" "echo ~@{6}~")

(call-process "bash" nil (current-buffer) nil "-c" "cat~@{6}~")
(call-process "bash" nil (current-buffer) nil "-c" "cat ~@{6}~")
(call-process "cat" nil (current-buffer) nil "~@{6}~")
(call-process "cat" nil (current-buffer) nil "z ~@{6}~")/usr/bin/cat: 'z ~@{6}~': No such file or directory


(call-process "bash" nil (current-buffer) nil "-c" "~@{6}~")
(call-process "bash" nil (current-buffer) nil "-c" "X~@{6}~")

(call-process "bash" nil (current-buffer) nil "-c" "{6}")

(compile "cat ~@{6}~")
(compile "cat {6}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ediff-exec-process (program buffer synch options &rest files)
  "Execute the diff PROGRAM.

The PROGRAM output is sent to BUFFER, which must be a live buffer
object.

The PROGRAM is executed asynchronously unless `system-type' is
`windows-nt' or `ms-dos', or SYNCH is non-nil.

OPTIONS is a string of space-separated options to pass to PROGRAM.  It
may be a blank string.

FILES is a list of filenames to pass to PROGRAM; nil and \"\" elements
are ignored."
  (let ((data (match-data))
	;; If this is a buffer job, we are diffing temporary files
	;; produced by Emacs with ediff-coding-system-for-write, so
	;; use the same encoding to read the results.
	(coding-system-for-read
	 (if (string-match "buffer" (symbol-name ediff-job-name))
	     ediff-coding-system-for-write
	   ediff-coding-system-for-read))
        (process-environment
         ;; Avoid localization of messages so we can parse the output.
         (cons "LC_MESSAGES=C" process-environment))
        args)
    (setq args (append (split-string options)
                       (mapcar (lambda (file)
                                 (when (stringp file)
                                   (file-name-unquote
                                    (or (file-local-copy file) file))))
                               files)))
    (setq args (delete "" (delq nil args))) ; delete nil and "" from arguments
    ;; the --binary option, if present, should be used only for buffer jobs
    ;; or for refining the differences
    (or (string-match "buffer" (symbol-name ediff-job-name))
	(eq buffer ediff-fine-diff-buffer)
	(setq args (delete "--binary" args)))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          ;; default-directory may be on some remote machine
          ;; (e.g. accessed via Tramp or url-handler) or a non-existing dir.
          (setq default-directory "/")
          (if (or (memq system-type '(ms-dos windows-nt))
                  synch)
              ;; In Windows do it synchronously, since Windows doesn't let us
              ;; delete files used by other processes. Thus, in ediff-buffers
              ;; and similar functions, we can't delete temp files because
              ;; they might be used by the asynch process that computes
              ;; custom diffs. So, we have to wait till custom diff
              ;; subprocess is done.
              ;; In DOS, must synchronize because DOS doesn't have
              ;; asynchronous processes.


;              (apply #'call-process program nil buffer nil args)
	      (call-process "bash" nil buffer nil "-c" (string-join (cons program args) " "))



            ;; On other systems, do it asynchronously.
            (let ((proc (get-buffer-process buffer)))
	      (if proc (kill-process proc)))
	    (let ((proc
		   (apply #'start-process "Custom Diff" buffer program args)))
	      (setq mode-line-process '(":%s"))
	      (set-process-sentinel proc #'ediff-process-sentinel)
	      (set-process-filter proc #'ediff-process-filter)
	      )))
      (store-match-data data))))

(string-join `("a" "b" "c") ",")

(defun ediff-exec-process (program buffer synch options &rest files)
  "Execute the diff PROGRAM.

The PROGRAM output is sent to BUFFER, which must be a live buffer
object.

The PROGRAM is executed asynchronously unless `system-type' is
`windows-nt' or `ms-dos', or SYNCH is non-nil.

OPTIONS is a string of space-separated options to pass to PROGRAM.  It
may be a blank string.

FILES is a list of filenames to pass to PROGRAM; nil and \"\" elements
are ignored."
  (let ((data (match-data))
	;; If this is a buffer job, we are diffing temporary files
	;; produced by Emacs with ediff-coding-system-for-write, so
	;; use the same encoding to read the results.
	(coding-system-for-read
	 (if (string-match "buffer" (symbol-name ediff-job-name))
	     ediff-coding-system-for-write
	   ediff-coding-system-for-read))
        (process-environment
         ;; Avoid localization of messages so we can parse the output.
         (cons "LC_MESSAGES=C" process-environment))
        args)
    (setq args (append (split-string options)
                       (mapcar (lambda (file)
                                 (when (stringp file)
                                   (file-name-unquote
                                    (or (file-local-copy file) file))))
                               files)))
    (setq args (delete "" (delq nil args))) ; delete nil and "" from arguments
    ;; the --binary option, if present, should be used only for buffer jobs
    ;; or for refining the differences
    (or (string-match "buffer" (symbol-name ediff-job-name))
	(eq buffer ediff-fine-diff-buffer)
	(setq args (delete "--binary" args)))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          ;; default-directory may be on some remote machine
          ;; (e.g. accessed via Tramp or url-handler) or a non-existing dir.
          (setq default-directory "/")
          (if (or (memq system-type '(ms-dos windows-nt))
                  synch)
              ;; In Windows do it synchronously, since Windows doesn't let us
              ;; delete files used by other processes. Thus, in ediff-buffers
              ;; and similar functions, we can't delete temp files because
              ;; they might be used by the asynch process that computes
              ;; custom diffs. So, we have to wait till custom diff
              ;; subprocess is done.
              ;; In DOS, must synchronize because DOS doesn't have
              ;; asynchronous processes.
              (apply #'call-process program nil buffer nil args)
            ;; On other systems, do it asynchronously.
            (let ((proc (get-buffer-process buffer)))
	      (if proc (kill-process proc)))
	    (let ((proc
		   (apply #'start-process "Custom Diff" buffer program args)))
	      (setq mode-line-process '(":%s"))
	      (set-process-sentinel proc #'ediff-process-sentinel)
	      (set-process-filter proc #'ediff-process-filter)
	      )))
      (store-match-data data))))
