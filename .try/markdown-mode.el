(setq markdown-command "pandoc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problems with ':' as a path component makes browsing file URLs of
;; w32 a problem. file-name-split fails because it uses general purpose
;; directory and file parsing which doesn't seem to work on windows
;
;; These overriding functions can workaround those issues
;;

(defun file-name-split (path) (unconcat path "[/\\]"))

(defun url-hexify-string-colon-ok (s)
  (let ((url-unreserved-chars (cons ?: url-unreserved-chars)))
    (url-hexify-string s)
    ))


(defun browse-url-file-url (file)
  "Return the URL corresponding to FILE.
Use variable `browse-url-filename-alist' to map filenames to URLs."
  (when-let ((coding (browse-url--file-name-coding-system)))
    (setq file (encode-coding-string file coding)))
  (if (and (file-remote-p file)
           ;; We're applying special rules for FTP URLs for historical
           ;; reasons.
           (seq-find (lambda (match)
                       (and (string-match-p (car match) file)
                            (not (string-match "\\`file:" (cdr match)))))
                     browse-url-filename-alist))
      (setq file (browse-url-url-encode-chars file "[*\"()',=;?% ]"))
    ;; Encode all other file names properly.
    (setq file (mapconcat #'url-hexify-string-colon-ok
                          (file-name-split file)
                          "/")))
  (dolist (map browse-url-filename-alist)
    (when (and map (string-match (car map) file))
      (setq file (replace-match (cdr map) t nil file))))
  file)

(top-level)

(browse-url-of-file "c:/Users/nick/AppData/Local/Temp/burlKTD3ag.html")


(w32-shell-execute "open" "c:/Users/nick/AppData/Local/Temp/burlKTD3ag.html")
(w32-shell-execute "open" "http://google.com")
(w32-shell-execute "open" "file://c:/Users/nick/AppData/Local/Temp/burlKTD3ag.html")
(w32-shell-execute "open" "file://c%3A/Users/nick/AppData/Local/Temp/burlKTD3ag.html")

(w32-shell-execute "open" "explorer" (concat "/e,/select," "e:/hub"))
(w32-shell-execute "open" "e:/hub")

(browse-url-file-url "e:/nick/AppData/Local/Temp/burlKTD3ag.html")
(eww-open-file "c:/Users/nick/AppData/Local/Temp/burlKTD3ag.html")

(markdown-live-preview-window-eww "c:/Users/nick/AppData/Local/Temp/burlKTD3ag.html")

(eww "file://c:/Users/nick/AppData/Local/Temp/burlKTD3ag.html")

(browse-url-url-encode-chars "e:/hub" "[*\"()',=;?% ]"))

(url-hexify-string-colon-ok "e:/foo")
(url-hexify-string "e:/foo")

(file-name-split "e:\\foo\\bar/baz")
