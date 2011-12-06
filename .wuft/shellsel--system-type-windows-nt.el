; specific to --system-type-windows-nt
;
; don't know what this is for really
; (getenv "SHELL") returns cmdproxy so it MAY be to do with
; switching between cmdproxy and bash
; it COULD also be a way of switching between cmd.exe and 
; the default shell for using cmd.exe for executing commands
; this would be more useful, but it doesn't look like it
; does this

(setq cmd-file-name (getenv "SHELL"))
(setq cmd-path (getenv "PATH"))

(setq bash-path (getenv "PATH"))

(defun bash-select ()
  (setenv "SHELL" bash-file-name)
  (setenv "PATH" bash-path)
  (setq exec-path (unconcat (getenv "PATH") ";"))
  )

(defun cmd-select ()
  (setenv "SHELL" cmd-file-name)
  (setenv "PATH" cmd-path)
  (setq exec-path (unconcat (getenv "PATH") ";"))
  )

;(bash-select)
(cmd-select)

