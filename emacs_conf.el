;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; add personal elisp directory to autoload
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The base conf directory
(setq emacs-repo-conf-dir (expand-file-name "~/.emacs_files/"))

;; For manually installed elisp
(setq emacs-repo-elisp-dir (expand-file-name 
			    (concat emacs-repo-conf-dir "elisp/")))

;; For source installs (no repo available to track)
(setq emacs-repo-elisp-src-dir (expand-file-name 
                                (concat emacs-repo-conf-dir "elisp_src/")))

;; For auto-install.el elisp
(setq emacs-repo-autoinst-elisp-dir (expand-file-name 
				     (concat emacs-repo-conf-dir "elisp_auto/")))

;; For git tracked submodules
(setq emacs-repo-elisp-submodule-dir 
      (expand-file-name 
       (concat emacs-repo-conf-dir "elisp_local/")))

;; As of emacs 23, ~/.emacs.d is user-emacs-directory
(setq custom-file (concat user-emacs-directory "emacs_conf_custom.el"))

(if (not (file-exists-p custom-file))
    (with-temp-buffer
      (write-file custom-file)))

(add-to-list 'load-path (expand-file-name emacs-repo-conf-dir))
(add-to-list 'load-path (expand-file-name emacs-repo-elisp-dir))

(setq mswindows-p (string-match "windows" (symbol-name system-type)))
(setq macosx-p (string-match "darwin" (symbol-name system-type)))
(setq linux-p (string-match "gnu/linux" (symbol-name system-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Make sure we have good ol' LISP available
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'compile)
(defvar *emacs-load-start* (current-time))

(setq lib-files
      (list
       ;; Basic emacs setup
       "emacs_conf_setup.el"

       ;; Before we do anything, pull all modules
       "emacs_conf_elget.el"
       custom-file

       ;; Mode setup and random externals
       "emacs_conf_exts.el"

       ;; Always load functions before binds, since we bind to functions 
       ;; somewhat often       
       "emacs_conf_funcs.el"

       "emacs_conf_org_mode.el"
       ;; "emacs_conf_wanderlust.el"
       "emacs_conf_erc.el"

       ;; Programming related stuff
       "emacs_conf_programming.el"
       "emacs_conf_ccmode.el"
       "emacs_conf_cedet.el"
       "emacs_conf_python.el"

       ;; bind as late as possible, so we already have everything in
       ;; that we're going to load
       "emacs_conf_binds.el"
       "emacs_conf_mud.el"
       "emacs_conf_automode.el"))

(mapcar 'load-library lib-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Time ourselves to see how long loading takes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "My .emacs loaded in %ds" 
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) 
              (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

;; Something in el-get is setting debug-on-error to t. 
;; Not cool.
(if debug-on-error
    (setq debug-on-error nil))

(org-agenda-list)
(delete-other-windows)