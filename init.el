
(add-hook 'after-init-hook
          `(lambda ()
             ;; toggling on debug-on-error just in case there's issues in my
             ;; untangled code
             (setq debug-on-error t)
             (org-babel-load-file "~/.emacs_files/emacs_conf.el")
             ;; if we live through startup, return to normal error throwing
             (setq debug-on-error nil)))
