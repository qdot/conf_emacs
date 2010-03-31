(when (file-exists-p (concat emacs-repo-elisp-submodule-dir "wanderlust/wl"))
  (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "wanderlust/wl"))
  (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "wanderlust/elmo"))
  (require 'wl)
)

