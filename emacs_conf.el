;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; add personal elisp directory to autoload
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_local/"))
(add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_local/magit/"))
(add-to-list 'load-path (expand-file-name "/usr/local/share/emacs/site-lisp/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; See if we're on MS Windows or Mac OS X
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar aquamacs-p (string-match "Aquamacs" (version)))


;; Basic emacs setup, personal functions and keybindings
(load-file "~/.emacs_files/emacs_conf_setup.el")
(load-file "~/.emacs_files/emacs_conf_funcs.el")
(load-file "~/.emacs_files/emacs_conf_binds.el")
(load-file "~/.emacs_files/emacs_conf_mac.el")

;; Mode setup
(load-file "~/.emacs_files/emacs_conf_exts.el")
(load-file "~/.emacs_files/emacs_conf_color_theme.el")
(load-file "~/.emacs_files/emacs_conf_ido.el")
(load-file "~/.emacs_files/emacs_conf_org_mode.el")
;;(load-file "~/.emacs_files/emacs_conf_wanderlust.el")

;; Programming related stuff
(load-file "~/.emacs_files/emacs_conf_programming.el")
(load-file "~/.emacs_files/emacs_conf_ccmode.el")

;; Check to see if we've checked out cedet, if so, load
(when (file-exists-p "~/.emacs_files/elisp_local/cedet/common/cedet.el")
  (load-file "~/.emacs_files/emacs_conf_cedet.el")
  (load-file "~/.emacs_files/emacs_conf_ede_home.el")
  (when (file-exists-p "~/.emacs_files/emacs_conf_ede_work.el")
    (load-file "~/.emacs_files/emacs_conf_ede_work.el")
    )
  )

;; Check to see if we've checked out ecb, if so, load
(when (file-exists-p "~/.emacs_files/elisp_local/ecb/ecb.el")
   (load-file "~/.emacs_files/emacs_conf_ecb.el")
)

(load-file "~/.emacs_files/emacs_conf_python.el")

(load-file "~/.emacs_files/emacs_conf_anything.el")