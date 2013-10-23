;; Ripping off emacs-starter-kit's init
;; Then making it more complicated because we want to make sure we use
;; the head of org-mode which we may have in an odd place.
;;

;; load from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
	  `(lambda ()
	     ;; Make sure we load our org-mode checkout first, otherwise shit happens.
	     (add-to-list 'load-path "~/.emacs_files/elisp_auto/org-mode/lisp/")
	     (require 'cl)
	     (require 'org)
       (org-babel-load-file "~/.emacs_files/emacs_conf.org"))

;;; init.el ends here
