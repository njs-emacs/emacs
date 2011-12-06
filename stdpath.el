; could canonicalize paths by replacing unc references to 
; local drives with local drive letters ?

(defun home-path (path) (filename-concat home-drive path))

(set-if-not-bound 'user-emacs-home (home-path "emacs"))
(set-if-not-bound 'backup-root (home-path "_backup"))
(set-if-not-bound 'home-daily-root (home-path "daily"))
(set-if-not-bound 'perl-e-path (home-path "perl"))
(set-if-not-bound 'devhome (home-path "aa"))
