(setq qdot/emacs-conf-dir (expand-file-name "~/.emacs_files/"))
(setq qdot/emacs-autoinst-elisp-dir (expand-file-name 
				     (concat qdot/emacs-conf-dir "elisp_auto/")))

(add-to-list 'load-path (expand-file-name (concat qdot/emacs-autoinst-elisp-dir "el-get")))
(setq el-get-dir qdot/emacs-autoinst-elisp-dir)
(setq el-get-verbose t)
(setq el-get-status-file (expand-file-name (concat qdot/emacs-conf-dir "org-schedule/elget-status-orgonly.el")))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(el-get)

(load-library (concat qdot/emacs-conf-dir "emacs_conf_org_mode.el"))
(setq org-agenda-sticky nil)
