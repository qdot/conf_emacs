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

(set-default-font "consolas-11")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Make sure we have good ol' LISP available
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defvar *emacs-load-start* (current-time))

;; Basic emacs setup, personal functions and keybindings
(load-file "~/.emacs_files/emacs_conf_setup.el")
(load-file "~/.emacs_files/emacs_conf_custom.el")
(load-file "~/.emacs_files/emacs_conf_funcs.el")
(load-file "~/.emacs_files/emacs_conf_binds.el")
(load-file "~/.emacs_files/emacs_conf_mac.el")
(load-file "~/.emacs_files/emacs_conf_apel.el")

;; Navigation and buffer setup
(load-file "~/.emacs_files/emacs_conf_anything.el")
(load-file "~/.emacs_files/emacs_conf_elscreen.el")

;; Mode setup
(load-file "~/.emacs_files/emacs_conf_exts.el")
(load-file "~/.emacs_files/emacs_conf_color_theme.el")
(load-file "~/.emacs_files/emacs_conf_ido.el")
(load-file "~/.emacs_files/emacs_conf_org_mode.el")
(load-file "~/.emacs_files/emacs_conf_wanderlust.el")

;; Programming related stuff
(load-file "~/.emacs_files/emacs_conf_programming.el")
(load-file "~/.emacs_files/emacs_conf_ccmode.el")
(load-file "~/.emacs_files/emacs_conf_python.el")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Time ourselves to see how long loading takes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))