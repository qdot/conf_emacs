(defvar *emacs-load-start* (current-time))

(setq qdot/emacs-conf-dir (file-name-directory (or load-file-name (buffer-file-name))))

(setq qdot/emacs-elisp-dir (expand-file-name
          (concat qdot/emacs-conf-dir "elisp/")))

(setq qdot/emacs-scripts-dir (expand-file-name
            (concat qdot/emacs-conf-dir "scripts/")))

(setq qdot/emacs-autoinst-elisp-dir (expand-file-name
             (concat qdot/emacs-conf-dir "elisp_auto/")))

(setq custom-file (concat user-emacs-directory "emacs_conf_custom.el"))
(if (not (file-exists-p custom-file))
    (with-temp-buffer
      (write-file custom-file)))

(add-to-list 'exec-path (expand-file-name qdot/emacs-scripts-dir))
(add-to-list 'load-path (expand-file-name qdot/emacs-conf-dir))
(add-to-list 'load-path (expand-file-name qdot/emacs-elisp-dir))

(org-babel-load-file "emacs_conf_setup.org")
(setq lib-files
      (list
       ;; Basic emacs setup. Anything that needs to happen before the
       ;; huge el-get package load happens here

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
       "emacs_conf_gnus.el"
       "emacs_conf_autocomplete.el"
       "emacs_conf_notify.el"

       ;; Programming related stuff
       "emacs_conf_programming.el"

       ;; bind/automodes as late as possible, so we already have
       ;; everything in that we're going to load
       "emacs_conf_binds.el"
       "emacs_conf_automode.el"))

(mapcar 'load-library lib-files)

(if debug-on-error
    (message "Something in init is setting debug-on-error to t. Fix it!")
    (setq debug-on-error nil))
