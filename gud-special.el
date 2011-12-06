(load "gud")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; behavioural corrections
;

(or (fboundp 'cygdrive-to-dos) (defun cygdrive-to-dos (x) x))

(defun gud-find-file (file)
  ;; Don't get confused by double slashes in the name that comes from GDB.
  (while (string-match "//+" file)
    (setq file (replace-match "/" t t file))
    )
  (setq file (cygdrive-to-dos file))
  (let ((minor-mode gud-minor-mode)
	(buf (funcall (or gud-find-file 'gud-file-name) file)))
    (when (stringp buf)
      (setq buf (and (file-readable-p buf) (find-file-noselect buf 'nowarn))))
    (when buf
      ;; Copy `gud-minor-mode' to the found buffer to turn on the menu.
      (with-current-buffer buf
	(set (make-local-variable 'gud-minor-mode) minor-mode)
	(set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
	(when (and gud-tooltip-mode
		   (memq gud-minor-mode '(gdbmi gdba)))
	  (make-local-variable 'gdb-define-alist)
	  (unless  gdb-define-alist (gdb-create-define-alist))
	  (add-hook 'after-save-hook 'gdb-create-define-alist nil t))
	(make-local-variable 'gud-keep-buffer))
      buf)))


(defun gdb-frame-handler ()
  "Sets `gdb-selected-frame' and `gdb-selected-file' to show
overlay arrow in source buffer."
  (gdb-delete-pending 'gdb-get-main-selected-frame)
  (let ((frame (bindat-get-field (gdb-json-partial-output) 'frame)))
    (when frame
      (setq gdb-selected-frame (bindat-get-field frame 'func))
      (setq gdb-selected-file 
	(or (bindat-get-field frame 'fullname)
	    (bindat-get-field frame 'file)))
      (setq gdb-frame-number (bindat-get-field frame 'level))
      (setq gdb-frame-address (bindat-get-field frame 'addr))
      (let ((line (bindat-get-field frame 'line)))
        (setq gdb-selected-line (or (and line (string-to-number line))
                                    nil)) ; don't fail if line is nil
        (when line ; obey the current file only if we have line info
          (setq gud-last-frame (cons gdb-selected-file gdb-selected-line))
          (gud-display-frame)))
      (if gud-overlay-arrow-position
          (let ((buffer (marker-buffer gud-overlay-arrow-position))
                (position (marker-position gud-overlay-arrow-position)))
            (when buffer
              (with-current-buffer buffer
                (setq fringe-indicator-alist
                      (if (string-equal gdb-frame-number "0")
                          nil
                        '((overlay-arrow . hollow-right-triangle))))
                (setq gud-overlay-arrow-position (make-marker))
                (set-marker gud-overlay-arrow-position position))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key gud-mode-map "\M-n" 'gud-next)
(define-key gud-mode-map "\M-s" 'gud-step)
(define-key gud-mode-map "\M-f" 'gud-finish)
(define-key gud-mode-map "\M-u" 'gud-up)
(define-key gud-mode-map "\M-d" 'gud-down)

(define-key gud-minor-mode-map "\M-n" 'gud-next)
(define-key gud-minor-mode-map "\M-s" 'gud-step)
(define-key gud-minor-mode-map "\M-f" 'gud-finish)
(define-key gud-minor-mode-map "\M-u" 'gud-up)
(define-key gud-minor-mode-map "\M-d" 'gud-down)
(define-key gud-minor-mode-map "\M-c" 'gud-cont)
(define-key gud-minor-mode-map "\M-r" 'gud-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extensions
;
;
;(setq gdb-exec-directory "e:/mi2ly.0.12/.bwg/")
;(setq gdb-exec-file "mi2ly.exe")

(setq gdb-gdb
  (case (wuft-get 'system-type)
    (gnu-linux "gdb --annotate=3")
    (windows-nt "d:/g/gdb-7.2/bin/gdb-python27 -i=mi")
    )
  )

(defun gdb-pop-to-buffer () (interactive)
  (cond
   ((get-buffer-process gud-comint-buffer) (pop-to-buffer gud-comint-buffer t))
   ((let ((default-directory gdb-exec-directory))
      (gdb (format "%s %s" gdb-gdb gdb-exec-file))
      )
    )
   )
  )

(define-key global-map [C-f9] 'gdb-pop-to-buffer)

(define-key gud-mode-map [f9]
  '(lambda () (interactive) (comint-send-string gud-comint-buffer "so .gdbinit\n"))
  )

(defun gdb-buffer-purge () (interactive)
  (mapcar '(lambda (x)
	     (cond ((string-match "e:/mi2ly.0.12/.*\\.c" (or (buffer-file-name x) ""))
		    (kill-buffer x)
		    )
		   )
	     )
	  (buffer-list)))

(define-key gud-mode-map [C-f9] 'gdb-buffer-purge)

(defun gdb-send-command (cmd)
  (comint-send-string gud-comint-buffer (format "%s\n" cmd))
  )

(top-level)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extra collected stuff
;

(defun gdb-source (args)
  (string-match gdb-source-spec-regexp args)
  ;; Extract the frame position from the marker.
  (setq gud-last-frame
	(cons
	 (gdb-fix-filename (match-string 1 args))
	 (string-to-number (match-string 2 args))))
  (setq gdb-pc-address (match-string 3 args))
  ;; cover for auto-display output which comes *before*
  ;; stopped annotation
  (if (eq gdb-output-sink 'inferior) (setq gdb-output-sink 'user)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(while (setq(string-match "/cygdrive/\\(.\\)" string)

(defadvice find-file (before tweep) (debug))
(ad-activate 'find-file)
(find-file "init.el")
(ad-unadvise 'find-file)

(defadvice gud-gdb-marker-filter (before yow) (debug))
(ad-activate 'gud-gdb-marker-filter)

(defadvice gud-gdbmi-marker-filter (before poom)
 (setq string (cygdrive-to-dos string))
 )

(ad-activate 'gud-gdbmi-marker-filter)
(ad-unadvise 'gud-gdbmi-marker-filter)

(eval "nopli")

string
