;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bake-set-path-clean ()
  (let* ((p (unconcat (getenv "PATH") ";"))
	 path
	 )
    (setq p (bake-path-clean p))
    (setenv "PATH" (setq path (mconcat p ";")))
    path
    )
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq wcc-home "d:\\w\\wcc")
(setq wcc-path '(
		 "d:\\w\\wcc\\binx"
		 "d:\\w\\wcc\\binnt"
		 "d:\\w\\wcc\\binw"
		 ))

(cond ((>= emacs-major-version 22)
       (setq compilation-error-regexp-alist-alist
	 (alist-put compilation-error-regexp-alist-alist 'wcc
		    '(
		      "^\\(.*?\\)(\\([0-9]+\\)"
		      1 2)))
       ))

(defun wcc-file (x)
  (filename-canonical x (getenv "WATCOM"))
  )

(defun bake-config-wcc ()
  (setenv "WATCOM" wcc-home)
  (setq wcc-path (mapcar '(lambda (x) (format "%s\\%s" wcc-home x))
			 '("binx" "binnt" "binw")))
  
  (let* ((p (unconcat (getenv "PATH") ";")
	    )
	 )
    (setq p (bake-path-clean p))
    (setq p (list-split-insert p wcc-path path-insertion-point))
    (setenv "PATH" (mconcat p ";"))

    (setq-default bake-program "wmake")
    (setq-default bake-dir-path '("." "__w" ".." "../__w"  "../../__w"))
    (setenv "MAKEFLAGS" "-HUCE")

    (default-frame-alist-put 'background-color "LightGray")
    (modify-frame-parameters (selected-frame) '((background-color . "LightGray")))

    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Studio 2008 (version 9)

(defun bake-config-msvc9 ()
  (setq ms-vs-home "D:\\VS9")
  (setenv "VSHOME" ms-vs-home) 

  (setq ms-vc-home (format "%s\\VC" ms-vs-home))

  (setq ms-sdk-home "d:\\SDK\\Windows\\v6.1")
  (setq ms-net-home "c:\\WINDOWS\\Microsoft.NET\\Framework\\v3.5")

  (setq ms-vc-path (list
		    (format "%s\\Common7\\tools\\bin" ms-vs-home)
		    (format "%s\\Common7\\IDE" ms-vs-home)
		    (format "%s\\bin" ms-vc-home)
		    (format "%s\\bin" ms-sdk-home)
		    ms-net-home
		    ))
  (let* ((p (unconcat (getenv "PATH") ";"))
	 )
    (setq p (bake-path-clean p))
    (setq p (list-split-insert p ms-vc-path path-insertion-point))
    (setenv "PATH" (mconcat p ";"))

    (setq-default bake-program "nmake /nologo")
    (setq-default bake-dir-path '("." "=v" "../=v"  "__v" "../__v" ".."))
    (setenv "MAKEFLAGS" nil t)
    (setenv "LIB"
	    (cat
	     (mapcar '(lambda (x) (format "%s\\%s" ms-vc-home x))
		     (list "atlmfc\\lib" "lib")) ";"))
    (setenv "INCLUDE"
	    (cat
	     (mapcar '(lambda (x) (format "%s\\%s" ms-vc-home x))
		     (list "atlmfc\\include" "PlatformSDK\\include" "include")
		     ) ";")
	    )

;    (default-frame-alist-put 'background-color "#D0C0C0")
;    (modify-frame-parameters (selected-frame) '((background-color . "#D8D0D0")))

    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Studio 2005 (version 8)

(defun bake-config-msvc8 ()
  (setenv "VSHOME" "D:\\VS8")

  (setq ms-vs-home (or (getenv "VSHOME") "d:\\VS9"))
  (setq ms-vc-home (format "%s\\VC7" ms-vs-home))

  (setq ms-sdk-home (format "%s\\PlatformSDK" ms-vc-home))
  (setq ms-net-home "c:\\WINDOWS\\Microsoft.NET\\Framework\\v3.5")

  (setq ms-vc-path (list
		    (format "%s\\Common7\\tools\\bin" ms-vs-home)
		    (format "%s\\Common7\\IDE" ms-vs-home)
		    (format "%s\\bin" ms-vc-home)
		    ms-net-home
		    ))

  (let* ((p (unconcat (getenv "PATH") ";"))
	 )
    (setq p (bake-path-clean p))
    (setq p (list-split-insert p ms-vc-path path-insertion-point))
    (setenv "PATH" (mconcat p ";"))

    (setq-default bake-program "nmake /nologo")
    (setq-default bake-dir-path '("." "__v" "../__v" ".."))
    (setenv "MAKEFLAGS" nil t)
    (setenv "LIB"
	    (cat
	     (mapcar '(lambda (x) (format "%s\\%s" ms-vc-home x))
		     (list "atlmfc\\lib" "lib")) ";"))
    (setenv "INCLUDE"
	    (cat
	     (mapcar '(lambda (x) (format "%s\\%s" ms-vc-home x))
		     (list "atlmfc\\include" "PlatformSDK\\include" "include")
		     ) ";")
	    )

;    (default-frame-alist-put 'background-color "#D0C0C0")
;    (modify-frame-parameters (selected-frame) '((background-color . "#D8D0D0")))

    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bake-config-gcc ()
  (let* ((p (unconcat (getenv "PATH") ";"))
	 )
    (setq p (bake-path-clean p))
;    (setq gcc-path nil)
;    (setq p (list-split-insert p gcc-path path-insertion-point))
    (setenv "PATH" (mconcat p ";"))

    (setq-default bake-program "make --no-print-directory")
    (setq-default bake-dir-path '("." "__g" ".." "../__g"  "../../__g"))
    (setenv "MAKEFLAGS" nil)

    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq path-insertion-point "d:\\cygwin\\bin")

;(bake-config-wcc)
;(bake-config-gcc)
;(bake-config-msvc)
;(bake-config-msvc9)

(defun path-show () (show (mconcat (unconcat (getenv "PATH") ";") "\n")))
;(path-show)

