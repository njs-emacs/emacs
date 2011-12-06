;#~obsolete

(setq gnus-nntp-server "news")
(gnus)

(defun nntp-open-server (host &optional service)
  "Open news server on HOST.
If HOST is nil, use value of environment variable `NNTPSERVER'.
If optional argument SERVICE is non-nil, open by the service name."
  (let ((host (or host (getenv "NNTPSERVER")))
	(status nil))
    (setq nntp-status-message-string "")
    (cond ((and host (nntp-open-server-internal host service))
	   (setq status (nntp-wait-for-response "^[23].*\r$"))
	   ;; Do check unexpected close of connection.
	   ;; Suggested by feldmark@hanako.stars.flab.fujitsu.junet.
	   (if status
	       (set-process-sentinel nntp-server-process
				     'nntp-default-sentinel)
	     ;; We have to close connection here, since function
	     ;;  `nntp-server-opened' may return incorrect status.
	     (nntp-close-server-internal)
	     ))
	  ((null host)
	   (setq nntp-status-message-string "NNTP server is not specified."))
	  )
    status
    ))

(defun nntp-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE (default is nntp)."
  (save-excursion
    ;; Use TCP/IP stream emulation package if needed.
    (or (fboundp 'open-network-stream)
	(require 'tcp))
    ;; Initialize communication buffer.
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-flush-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    (setq nntp-server-process
	  (open-network-stream "nntpd" (current-buffer)
			       host (or service "nntp")))
    (setq nntp-server-name host)
    ;; It is possible to change kanji-fileio-code in this hook.
    (run-hooks 'nntp-server-hook)
    ;; Return the server process.
    nntp-server-process
    ))

(defun nntp-accept-response ()
  (or (memq (process-status nntp-server-process) '(open run))
      (error "NNTP: Connection closed."))
  (condition-case errorcode
      (accept-process-output nntp-server-process)
    (error
     (cond ((string-equal "select error: Invalid argument" (nth 1 errorcode))
	    nil
	    )
	   (t
	    (signal (car errorcode) (cdr errorcode))))
     ))
  )

(defun process-wait (process regexp)
  (save-excursion
    (let ((buf (process-buffer process)))
      (set-buffer buf)
      (while (progn (bob) (not (re-search-forward regexp nil t)))
	(accept-process-output process)
	)))
  )

(defun nntp-wait (regexp)
  (process-wait nntp-server-process regexp))

(defun nntp-send (s &optional response)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (process-send-string nntp-server-process s)
    (process-send-string nntp-server-process "\n")
    (and response (nntp-wait response))
    ))

(defun nntp-kill ()
  (and nntp-server-process (delete-process nntp-server-process))
  (and nntp-server-buffer (kill-buffer nntp-server-buffer))
  (setq nntp-server-buffer nil)
  (setq nntp-server-process nil))

(nntp-kill)
(list-processes)
(nntp-open-server "news")
  
(nntp-send "GROUP alt.2600")
(nntp-send "xhdr Subject 5117-6273")
(nntp-send "xhdr Subject 6117-6273")
(nntp-send "body 5997-5998")



(defun article (n)
  (nntp-send (format "article %s" n) "^\\.\r$"))

(dotimes (i (- 6273 5117))
  (article (+ 5117 i))
  (re)
  )


(nntp-send-command "" "GROUP" "alt.2600")

(nntp-send-command "^\\.\r$" "article 5118")
(nntp-send-command "^\\.\r$" "xhdr Subject 5117-6273")

(nntp-send-command "^\\.\r$" "head")
(nntp-send-command "^\\.\r$" "body")
(nntp-send-command "^[23].*\r$" "next")

(nntp-send-command "^\\.\r$" "head")

(nntp-send-strings-to-server "next")

(dotimes (i 11) (nntp-send-strings-to-server (format "article %d" (+ 70 i))))

(defun news-foo () (interactive)
  (nntp-send-command "^[23].*\r$" "next")
  (nntp-send-strings-to-server "body")
  )



