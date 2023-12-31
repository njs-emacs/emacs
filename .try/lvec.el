; this is a feature that allows for an internal version sequencing
; system that can be used to synchronise elements with external
; sequences like manual sections etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lvec gives a vector of section/version/level numbers
; 

(defun lvec-increment (n) (interactive "p")
  (setcar (nthcdr n lvec) (1+ (nth n lvec)))
  (message "version is now %s" (lvec-format))
  )

(defun lvec-reset (n) (interactive "p")
  (setcar (nthcdr n lvec) (nth n lvec-base))
  (message "version is now %s" (lvec-format))
  )

(defun lvec-increment-carry (n) (interactive "p")
  (setcar (nthcdr n lvec) (1+ (nth n lvec)))
  (message "version is now %s" (lvec-format))
  (while (< (setq n (1+ n)) (length lvec))
    (lvec-reset n)
    )
  )

(defvar-local lvec-base nil "lvec")
(defvar-local lvec-save nil "lvec")
(defvar-local lvec '(0 0 0 0) "lvec")

(defvar-local lvec-format-list '("%s" "%s" "%s" "%c") "lvec")
(defvar-local lvec-format-template "%s_%s_%s%s" "lvec")

(set-default 'lvec-format-list '("%s" "%s" "%s" "%c"))
(set-default 'lvec-format-template "%s_%s_%s%s")

(defun lvec-format-one (n &optional value)
  (format (nth n lvec-format-list) (or value (nth n lvec)))
  )

(defun lvec-format (&optional format)
  (setq format (or format (apply 'format lvec-format-template lvec-format-list)))
  (apply 'format format lvec)
  )

(defun lvec-set (&rest args)
  (setq lvec-base args)
  )

(defun lvec-reset (&rest args)
  (cond (args (setq lvec-base args)))
  (setq lvec (copy-list lvec-base))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lvec-format-list '("%s" "%s" "%s" "%c"))
(setq lvec-format-template "%s_%s_%s%s")

(defun lvec-insert () (interactive)
  (insert (lvec-format))
  )

(defun lvec-defhydra () (interactive)
       (defhydra lvec-hydra (:color pink)
      "LVec %(format \"%s\" lvec) %(lvec-format)\n
"
  ("q" (setcar (nthcdr 0 lvec) (1+ (nth 0 lvec))) (lvec-format-one 0 (1+ (nth 0 lvec))) :column "Increment:")
  ("w" (setcar (nthcdr 1 lvec) (1+ (nth 1 lvec))) (lvec-format-one 1 (1+ (nth 1 lvec))))
  ("e" (setcar (nthcdr 2 lvec) (1+ (nth 2 lvec))) (lvec-format-one 2 (1+ (nth 2 lvec))))
  ("r" (setcar (nthcdr 3 lvec) (1+ (nth 3 lvec))) (lvec-format-one 3 (1+ (nth 3 lvec))))
;  ("t" (setcar (nthcdr 4 lvec) (1+ (nth 4 lvec))) (lvec-format-one 4 (1+ (nth 4 lvec))))

  ("a" (setcar (nthcdr 0 lvec) (1- (nth 0 lvec))) (lvec-format-one 0 (1- (nth 0 lvec))) :column "Decrement:")
  ("s" (setcar (nthcdr 1 lvec) (1- (nth 1 lvec))) (lvec-format-one 1 (1- (nth 1 lvec))))
  ("d" (setcar (nthcdr 2 lvec) (1- (nth 2 lvec))) (lvec-format-one 2 (1- (nth 2 lvec))))
  ("f" (setcar (nthcdr 3 lvec) (1- (nth 3 lvec))) (lvec-format-one 3 (1- (nth 3 lvec))))
;  ("g" (setcar (nthcdr 4 lvec) (1- (nth 4 lvec))) (lvec-format-one 4 (1- (nth 4 lvec))))

  ("z" (setcar (nthcdr 0 lvec) (nth 0 lvec-base)) :column "Zero:")
  ("x" (setcar (nthcdr 1 lvec) (nth 1 lvec-base)))
  ("c" (setcar (nthcdr 2 lvec) (nth 2 lvec-base)))
  ("v" (setcar (nthcdr 3 lvec) (nth 3 lvec-base)))
;  ("b" (setcar (nthcdr 4 lvec) (nth 4 lvec-base)))

  ("g" () "quit" :exit t :column "Other:")
  ("/" (setq lvec lvec-base) "revert")

  ))

; (lvec-format)
;  (lvec-format-one 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-key global-map (kbd "s-e h") 'lvec-defhydra)
(def-key global-map (kbd "s-e h") 'lvec-hydra/body)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;; (setq lvec-base `(1 1 1 ?a))
;; (setq lvec-format-list '("%s" "%s" "%s" "%c"))
;; (setq lvec-format-template "%s_%s_%s%s")
;; 
;; 

