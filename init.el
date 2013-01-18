;; Ripping off emacs-starter-kit's init
;; Then making it more complicated because we want to make sure we use
;; the head of org-mode which we may have in an odd place.
;;

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq qdot/emacs-conf-dir ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; load up the starter kit
		(add-to-list 'load-path (concat qdot/emacs-conf-dir "elisp_auto/org-mode"))
    (require 'org)
    (org-babel-load-file (expand-file-name "emacs_conf.org" qdot/emacs-conf-dir))))

;;; init.el ends here
