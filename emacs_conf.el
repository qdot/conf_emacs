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
(add-to-list 'load-path (expand-file-name emacs-repo-autoinst-elisp-dir))
(add-to-list 'load-path (expand-file-name emacs-repo-autoinst-elisp-dir))
(add-to-list 'load-path (expand-file-name emacs-repo-elisp-submodule-dir))

;; (setq warning-suppress-types nil)
;; (setq byte-compile-warnings nil)
;; (setq byte-compile-verbose nil)

;; (require 'byte-code-cache)
;; (add-to-list 'bcc-blacklist "emacs_conf_cedet.el$")
;; (add-to-list 'bcc-blacklist "tokochiku.*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Platforms and fonts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mswindows-p (string-match "windows" (symbol-name system-type)))
(setq macosx-p (string-match "darwin" (symbol-name system-type)))
(setq linux-p (string-match "gnu/linux" (symbol-name system-type)))

;; We know we have consolas on OS X, so use it
;; We also need to do this as near the beginning as possible, since it crashes
;; otherwise?
(if macosx-p
    (set-face-font 'default "consolas-11")
  )
(if mswindows-p
    (set-face-font 'default "consolas-8")
  )
;; (if linux-p
;;     ;;(set-face-font 'default "inconsolata-11")
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Make sure we have good ol' LISP available
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defvar *emacs-load-start* (current-time))

(setq lib-files
      (list 
       ;; Basic emacs setup, personal functions and keybindings
       "emacs_conf_setup.el"
       custom-file

       ;; Always load functions before binds, since we bind to functions 
       ;; somewhat often       
       "emacs_conf_funcs.el"

       ;; Mac crap
       "emacs_conf_mac.el"

       ;; Japan!
       "emacs_conf_apel.el"

       ;; Make sure we pick up autoinstall
       "emacs_conf_autoinstall.el"

       ;; Windows mode (for restoring buffer configurations)
       "emacs_conf_windows.el"

       ;; Mode setup and random externals
       "emacs_conf_exts.el"
       "emacs_conf_org_mode.el"
       "emacs_conf_wanderlust.el"
       "emacs_conf_erc.el"

       ;; Programming related stuff
       "emacs_conf_programming.el"
       "emacs_conf_ccmode.el"
       "emacs_conf_python.el"
       "emacs_conf_cedet.el"
       "emacs_conf_ecb.el"

       ;; bind as late as possible, so we already have everything in
       ;; that we're going to load
       "emacs_conf_binds.el"
       "emacs_conf_mud.el"
       )
      )

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
