;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; add personal elisp directory to autoload
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The base conf directory
(if load-in-progress
    (setq qdot/emacs-conf-dir (file-name-directory load-file-name))
	(setq qdot/emacs-conf-dir (file-name-directory buffer-file-name)))

;; For manually installed elisp
(setq qdot/emacs-elisp-dir (expand-file-name 
														(concat qdot/emacs-conf-dir "elisp/")))

;; For manually installed elisp
(setq qdot/emacs-scripts-dir (expand-file-name 
															(concat qdot/emacs-conf-dir "scripts/")))

;; For source installs (no repo available to track)
(setq qdot/emacs-elisp-src-dir (expand-file-name 
                                (concat qdot/emacs-conf-dir "elisp_src/")))

;; For auto-install.el elisp
(setq qdot/emacs-autoinst-elisp-dir (expand-file-name 
																		 (concat qdot/emacs-conf-dir "elisp_auto/")))

;; As of emacs 23, ~/.emacs.d is user-emacs-directory
(setq custom-file (concat user-emacs-directory "emacs_conf_custom.el"))

(if (not (file-exists-p custom-file))
    (with-temp-buffer
      (write-file custom-file)))

(add-to-list 'exec-path (expand-file-name qdot/emacs-scripts-dir))
(add-to-list 'load-path (expand-file-name qdot/emacs-conf-dir))
(add-to-list 'load-path (expand-file-name qdot/emacs-elisp-dir))

(setq mswindows-p (string-match "windows" (symbol-name system-type)))
(setq macosx-p (string-match "darwin" (symbol-name system-type)))
(setq linux-p (string-match "gnu/linux" (symbol-name system-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Make sure we have good ol' LISP available
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
;; We need compile before CEDET, and compile+ before compile, so just
;; do compile+, which also picks up compile- in the correct order. Oi.
(require 'compile+)
(defvar *emacs-load-start* (current-time))

;; Load cedet first, otherwise we'll conflict against the 1.1 repo stuff
(load-file (concat qdot/emacs-autoinst-elisp-dir "cedet/cedet-devel-load.el"))

(setq lib-files
      (list
       ;; Basic emacs setup. Anything that needs to happen before the
       ;; huge el-get package load happens here
       "emacs_conf_setup.el"

       ;; Pull all modules now
       "emacs_conf_elget.el"
       custom-file

       ;; Mode setup and random externals
       "emacs_conf_exts.el"

       ;; Always load functions before binds, since we bind to functions 
       ;; somewhat often       
       "emacs_conf_funcs.el"

       ;; Package specific setup
       "emacs_conf_wg.el"
       "emacs_conf_org_mode.el"
       "emacs_conf_erc.el"
       "emacs_conf_autocomplete.el"

       ;; Programming related stuff
       "emacs_conf_programming.el"

       ;; bind/automodes as late as possible, so we already have
       ;; everything in that we're going to load
       "emacs_conf_binds.el"
       "emacs_conf_automode.el"))

(mapcar 'load-library lib-files)

;; Something in el-get is setting debug-on-error to t. 
;; Not cool.
(if debug-on-error
		(message "Something in init is setting debug-on-error to t. Fix it!")
    (setq debug-on-error nil))
