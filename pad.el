(setq kp-names `(
		 nil
		 kp-end
		 kp-down
		 kp-next
		 kp-left
		 kp-space
		 kp-right
		 kp-home
		 kp-up
		 kp-prior
		 ))

(defun pad-map (binding key &optional mods fun)
  (cond
   ((fboundp fun))
   (fun (setq fun 'local-set-key))
   ((setq fun 'global-set-key)))
  (cond
   ((eq mods 'a)
    (setq key (format "s-%d" key)))
   ((eq mods 'b)
    (setq key (format "<H-%s>" (nth key kp-names))))
   ((eq mods 'ab)
    (setq key (format "<s-%s>" (nth key kp-names))))
   ((setq key (format "H-%d" key)))
   )
  (funcall fun (read-kbd-macro key) binding)
  )

