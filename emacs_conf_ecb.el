(if (not (file-exists-p (concat emacs-repo-elisp-submodule-dir "ecb")))
    (message "ECB not found. Not loading.")
  (progn 
    (add-to-list 'load-path
                 "~/.emacs_files/elisp_local/ecb/")
    (load-file "~/.emacs_files/elisp_local/ecb/ecb.el")
    (require 'ecb) 
    (setq ecb-options-version "2.40")
    )
  )

