;; Setting up load directories for apel/flim/semi that a lot of japanese emacs modules use

;; We must require mailcap /before/ flim to make sure we get the right one, 
;; since it comes with a super old version


(require 'mailcap)

(when (file-exists-p (concat emacs-repo-elisp-submodule-dir "apel/"))
  (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "apel/"))
)

(when (file-exists-p (concat emacs-repo-elisp-submodule-dir "flim/"))
  (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "flim/"))
)

(when (file-exists-p (concat emacs-repo-elisp-submodule-dir "semi/"))
  (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "semi/"))
)