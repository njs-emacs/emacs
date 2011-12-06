(require 'bake)
(provide 'umake)

(defun umake-bake-select (&optional build)
  (or build (setq build
	      (case (wuft-get 'system-type)
		(gnu-linux ".bln")
		(windows-nt ".bwg")
		)
	      )
      )
  (and build (bake-put 'build build))
  )

(defun umake-bake-register ()
  (setq umake-bake-info
    `(
      (path . (
	       ,(expand-file-name (filename-concat devhome "__tool/bin"))
	       ))
      (build . ".bwg")
      (build-files . ("umake.pl" "makefile"))
      (command . ,(case (wuft-get 'system-type)
		    (gnu-linux "umake.pl")
;		  (windows-nt "perl d:/bin/umake.pl")
		    (windows-nt "perl -S umake-main.pl")
		    ))
      (env . (
	      ("PERLLIB" . (expand-file-name (filename-concat devhome "__tool")))
	      ))
      (select . umake-bake-select)
      )
    )

  (bake-tool-register 'umake umake-bake-info)
  )

(umake-bake-register)

(defun umake-change-devhome (new)
  (bake-tool-select 'null)
  (setq devhome new)
  (setenv "DEVHOME" devhome)
  (umake-bake-register)
  (bake-tool-select 'umake)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq umake-preferred-build "")
(setq umake-preferred-root "")

(defun umake-file-locate (name &optional dir)
  (let* ((default-directory (or dir default-directory))
	 (path
	  (expand-file-name
	   (locate-file-in-path
	   name
	   (list "." ".." 
		 umake-preferred-build
		 (format "../%s" umake-preferred-build)
		 umake-preferred-root
		 (format "%s/%s" umake-preferred-root umake-preferred-build)
		 )
	   ))
	  )
	 )
    path
    ))

(setq umake-file-history nil)
(setq umake-file-map (copy-keymap minibuffer-local-completion-map))
(setq umake-file-jump-map nil)

(defun lookup-secondary-map (map &optional offset)
  "Lookup the current command in a secondary keymap according to trailing keys in command"
  (let* ((keys (this-command-keys))
	 (key (substring keys (or offset 1)))
	 )
    (lookup-key map key)))
  

(defun umake-file-map-replace (interactive)
  (let* ((x (lookup-secondary-map umake-file-jump-map)))
    (minibuffer-reset (eval x))
    )
  )

(defun umake-define-key (key binding)
  (define-key umake-file-map (format "%c%c" 27 ?1) 'umake-file-map-replace)
  (qb-define (control-key-vector ?m key) (format "%s/umake.pl" binding t))
  )

(defun umake-file-locate-here (&optional dir)
  (let* ((path (umake-file-locate "umake.pl" dir)))
    path)
  )
  
(defun umake-find-file (&optional dir)
  (interactive
    (list (read-from-minibuffer
	   "Dir: " (car umake-file-history) umake-file-map
	   nil '(umake-file-history . 1)
	   )
	  ))
  (let ((path (umake-file-locate "umake.pl" dir)))
    (cond (path (find-file path))
	  (t (error "Can't find umake.pl"))
	  )
    )
  )

;(define-key global-map (control-key-vector ?x ?m ?m) 'umake-find-file)
;(qb-define (control-key-vector ?m ?m) '(umake-file-locate-here))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(qb-define (control-key-vector ?m ?e) "umake.el" t)

