;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; add personal elisp directory to autoload
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq emacs-repo-conf-dir (expand-file-name "~/.emacs_files/"))
(setq emacs-repo-elisp-dir (expand-file-name 
                             (concat emacs-repo-conf-dir "elisp/")))
(setq emacs-repo-elisp-submodule-dir 
      (expand-file-name 
       (concat emacs-repo-conf-dir "elisp-local/")))
(setq emacs-local-dir (expand-file-name "~/emacs.d/"))

(add-to-list 'load-path (expand-file-name emacs-repo-conf-dir))
(add-to-list 'load-path (expand-file-name emacs-repo-elisp-dir))
(add-to-list 'load-path (expand-file-name emacs-repo-elisp-submodule-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; See if we're on MS Windows or Mac OS X
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mswindows-p (string-match "windows" (symbol-name system-type)))
(setq macosx-p (string-match "darwin" (symbol-name system-type)))
(setq linux-p (string-match "gnu/linux" (symbol-name system-type)))

;; We know we have consolas on OS X, so use it
;; We also need to do this as near the beginning as possible, since it crashes
;; otherwise?
(if macosx-p
    (set-frame-font "consolas-11")
  )
(if mswindows-p
    (set-frame-font "consolas-8")
  )
(if linux-p
    (set-frame-font "inconsolata-9")
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Make sure we have good ol' LISP available
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defvar *emacs-load-start* (current-time))

;; Basic emacs setup, personal functions and keybindings
(load-library "emacs_conf_setup.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; When custom decides to add things, have it do so in something other than
;; .emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat emacs-local-dir "emacs_conf_custom.el"))
(when (file-exists-p custom-file)
      (load-library "emacs_conf_custom.el")
)

;; Always load functions before binds, since we bind to functions somewhat often
(load-library "emacs_conf_funcs.el")

(load-library "emacs_conf_mac.el")
(load-library "emacs_conf_apel.el")

;; Mode setup
(load-library "emacs_conf_exts.el")
(load-library "emacs_conf_org_mode.el")

;; (when (file-exists-p 
;; "elisp_local/wanderlust/wl/wl-news.el")
;;   (load-library "emacs_conf_wanderlust.el")
;;   )

;; Programming related stuff
(load-library "emacs_conf_programming.el")
(load-library "emacs_conf_ccmode.el")
(load-library "emacs_conf_python.el")

;; Check to see if we've checked out cedet, if so, load 
(when (file-exists-p (concat emacs-repo-elisp-submodule-dir "cedet/"))
  (load-library "emacs_conf_cedet.el")
  (load-library "emacs_conf_ede_home.el")
  (when (file-exists-p "emacs_conf_ede_work.el")
    (load-library "emacs_conf_ede_work.el")
    )
  )

;; Check to see if we've checked out ecb, if so, load 
;;(when (file-exists-p "elisp_src/ecb-2.40/ecb.el") 
;;   (load-library "emacs_conf_ecb.el")
;;)

;; bind as late as possible, so we already have everything in
;; that we're going to load
(load-library "emacs_conf_binds.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Time ourselves to see how long loading takes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "My .emacs loaded in %ds" 
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) 
              (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
