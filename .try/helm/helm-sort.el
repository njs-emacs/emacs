(define-advice helm-list-directory (:override (directory) add-sort-by-kind)
  (let ((files (directory-files directory t directory-files-no-dot-files-regexp)))
    (pcase helm-ff-initial-sort-method
      ('nil
       files)
      ('newest
       (sort files #'file-newer-than-file-p))
      ('size
       (sort files #'helm-ff-file-larger-that-file-p))
      ('kind
       (let-alist (seq-group-by #'file-directory-p files)
         (nconc
          .t                            ; folders
          (sort
           .nil                         ; files
           (lambda (f1 f2)
             (let ((ext1 (or (file-name-extension f1) ""))
                   (ext2 (or (file-name-extension f2) "")))
               (if (equal ext1 ext2)
                   (string< f1 f2)
                 (string< ext1 ext2)))))))))))

(defun helm-ff-sort-by-kind ()
  (interactive)
  (unless (eq helm-ff-initial-sort-method 'kind)
    (setq helm-ff-initial-sort-method 'kind)
    (helm-force-update (helm-get-selection nil helm-ff-transformer-show-only-basename)))
  (message "Sorting by kind"))

(put 'helm-ff-sort-by-kind 'helm-only t)

(define-key helm-find-files-map (kbd "S-<f4>") #'helm-ff-sort-by-kind)


(setq helm-ff-initial-sort-method 'size)
(setq helm-ff-initial-sort-method 'newest)
(setq helm-ff-initial-sort-method 'kind)

;
