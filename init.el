;; Ripping off emacs-starter-kit's init
;; Then making it more complicated because we want to make sure we use
;; the head of org-mode which we may have in an odd place.
;;

;; load from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    (require 'org)
    (org-babel-load-file (expand-file-name "emacs_conf.org" ,(file-name-directory (or load-file-name (buffer-file-name)))))))

;;; init.el ends here
