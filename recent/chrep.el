(defun chrep (pat) (interactive "SCommand: ")
  (dregf pat "-hist.el" "ever")
  )

(defun chreph (pat) (interactive "STopic: ")
  (setq pat (format "(describe-(function|variable|key)|where-is).*%s" pat))
  (dregf pat "-hist.el" "ever" "--ho=hoho")
  )

;(chreph "avy")

