;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flymake + pyflakes syntax checking for python
;; http://plope.com/Members/chrism/flymake-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO This freaks out if pyflakes isn't available on the system
(when macosx-p
    ;;(setq flyflakes-pyflakes-command '("/Library/Frameworks/Python.framework/Versions/Current/bin/pyflakes")))
    (setq flyflakes-pyflakes-command '("/opt/homebrew/Cellar/python/2.7/bin/pyflakes")))
(require 'flyflakes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pylookup for python documentation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ======================================================================
;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
;; (setq pylookup-dir "~/.emacs_files/elisp_local/pylookup")
;; (add-to-list 'load-path pylookup-dir)

;; ;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))

;; ;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)

;; (autoload 'pylookup-update "pylookup" 
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pymacs and ropemacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path (expand-file-name (concat emacs-repo-elisp-submodule-dir "pymacs")))

;; (defun qdot/ac-config-python ()
;;   (ac-ropemacs-require)
;;   (setq ac-sources (append '(ac-source-yasnippet ac-source-ropemacs) ac-sources))
;;   )

;; (defun load-python-ac-reqs()
;;   (require 'pymacs)
;;   (autoload 'pymacs-apply "pymacs")
;;   (autoload 'pymacs-call "pymacs")
;;   (autoload 'pymacs-eval "pymacs" nil t)
;;   (autoload 'pymacs-exec "pymacs" nil t)
;;   (autoload 'pymacs-load "pymacs" nil t)
;;   ;;(eval-after-load "pymacs"
;;   ;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
;;   ;;(pymacs-load "ropemacs" "rope-")
;;   (setq ropemacs-enable-autoimport t))


(defun my-python-mode-hook()
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (set-variable 'indent-tabs-mode nil)
  (set-variable 'tab-width 4)
  (set-variable 'py-indent-offset 4)
  (lambda () (eldoc-mode 1))
  (local-set-key "\C-ch" 'pylookup-lookup)
  )

;; (add-hook 'python-mode-hook 'qdot/ac-config-python)
(add-hook 'python-mode-hook 'my-python-mode-hook)