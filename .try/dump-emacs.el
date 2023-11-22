; looks like we would need the full emacs build environment as dump-emacs
; does not seem to be part of the standard release

; https://www.emacswiki.org/emacs/DumpingEmacs

(defun my-dump-emacs ()
  (interactive)
  (load "e:/projects/emacs-config/install.el")
  (start-process "dump-emacs" "*dump*" "cmd" "/c" "cd" "/d" "E:\\dev-env\\emacs-src\\git\\emacs\\src"
                 "&&" ".\\oo-spd\\i386\\temacs.exe" "-batch" "-l" "loadup" "bootstrap")
  (with-current-buffer "*dump*"
    (viper-mode))
  (switch-to-buffer "*dump*")
  (labels ((%sentinel (process event)
             (when (equal 'exit (process-status process))
               (when (= 0 (process-exit-status process))
                 (let ((tmp-path "d:/emacs/bin/emacs.exe2"))
                   (when (file-exists-p tmp-path)
                     (condition-case nil
                          (delete-file tmp-path)
                        (error (setq tmp-path "d:/emacs/bin/emacs.exe3")
                               (when (file-exists-p tmp-path)
                                 (delete-file tmp-path)))))
                   (rename-file "d:/emacs/bin/emacs.exe" tmp-path))
                 (copy-file "E:\\dev-env\\emacs-src\\git\\emacs\\src\\oo-spd\\i386\\emacs.exe"
                            "d:/emacs/bin/emacs.exe" t)))))
    (set-process-sentinel (get-process "dump-emacs") '%sentinel)))
