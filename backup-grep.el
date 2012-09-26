; previously in $BACKUPHOME..meta/emacs.el
; use findgrep to find patterns in backed-up files
;
; ebg finds patterns in the latest versions of recently edited files

(defun backup-grep (pat &rest args)
  "Search for PAT in backed up files.\nUses findgrep, so has similar usage to ff functions."
  (let ((default-directory (filename-concat backup-root ".meta/")))
    (compile
     (format "perl ./backupgrep.pl -pat=\"%s\" %s" pat (mconcat args " "))
     )
    )
  )

