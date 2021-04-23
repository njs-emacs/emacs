(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

package-archives
(package-list-packages)

(setq package-archives
  `(
;    ("marmalade" . "http://marmalade-repo.org/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.org/packages/")
    )
  )
