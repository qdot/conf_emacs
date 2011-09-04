;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flymake + pyflakes syntax checking for python
;; http://plope.com/Members/chrism/flymake-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO This freaks out if pyflakes isn't available on the system
(if macosx-p
    ;;(setq flyflakes-pyflakes-command '("/Library/Frameworks/Python.framework/Versions/Current/bin/pyflakes")))
    (setq flyflakes-pyflakes-command '("/opt/homebrew/Cellar/python/2.7/bin/pyflakes"))
  (setq flyflakes-pyflakes-command '("/usr/local/bin/pyflakes")))
(require 'flyflakes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pymacs and ropemacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize Pymacs
(require 'pymacs)

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(ac-ropemacs-require)

(defun my-python-mode-hook()
  (font-lock-mode 1)
  (linum-mode 1)
  (font-lock-fontify-buffer)
  (set-variable 'indent-tabs-mode nil)
  (set-variable 'tab-width 4)
  (set-variable 'py-indent-offset 4)
  (lambda () (eldoc-mode 1))
  (local-set-key "\C-ch" 'pylookup-lookup)
  (auto-complete-mode nil)
  (set (make-local-variable 'ac-sources)
       (setq ac-sources (append '(ac-source-ropemacs) ac-sources))
       )
  (set (make-local-variable 'ac-find-function) 'ac-python-find)
  )

;; (add-hook 'python-mode-hook 'qdot/ac-config-python)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(remove-hook 'python-mode-hook 'wisent-python-default-setup)
