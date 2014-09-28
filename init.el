;; Ripping off emacs-starter-kit's init
;; Then making it more complicated because we want to make sure we use
;; the head of org-mode which we may have in an odd place.
;;
;; load from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
          `(lambda ()
             ;; toggling on debug-on-error just in case there's issues in my
             ;; untangled code
             (toggle-debug-on-error)
             ;; Make sure we load our org-mode checkout first, otherwise shit happens.
             (add-to-list 'load-path "~/.emacs_files/packages/org-plus-contrib.20140922/lisp/")
             (require 'cl)
             (require 'org)
             (require 'ob-tangle)
             (org-babel-load-file "~/.emacs_files/emacs_conf.org")
             ;; if we live through startup, return to normal error throwing
             (toggle-debug-on-error)))

;;; init.el ends here
