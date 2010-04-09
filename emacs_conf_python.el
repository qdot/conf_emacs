(require 'python)

(setq auto-mode-alist
      (append
       '(
         ("\\.py$"   . python-mode)
         ) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; insert 'self.' in front of member vars using c-;
;; http://nflath.com/2009/08/python-mode-customizations/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-insert-self ()
  "Insert self. at the beginning of the current expression."
  (interactive)
  (save-excursion
    (search-backward-regexp "[ \n\t,(-]\\|^")
    (if (not (looking-at "^"))
        (forward-char))
    (insert "self.")))
(define-key	python-mode-map	(kbd "C-;")	'my-insert-self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flymake + pyflakes syntax checking for python
;; http://plope.com/Members/chrism/flymake-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not provided by flymake itself, curiously
(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-python")))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-in-system-tempdir))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/usr/local/bin/pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pylookup for python documentation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ======================================================================
;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
(setq pylookup-dir "~/.emacs_files/elisp_local/pylookup")
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup" 
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
;;----------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pymacs and ropemacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_src/Pymacs-0.24-beta1"))

(require 'pymacs)
(add-to-list 'pymacs-load-path 
             (expand-file-name "~/.emacs_files/elisp_src/ropemacs-0.6/ropemacs"))
(add-to-list 'pymacs-load-path              (expand-file-name "~/.emacs_files/elisp_src/ropemacs-0.6/ropemode"))
(add-to-list 'pymacs-load-path              (expand-file-name "~/.emacs_files/elisp_src/Pymacs-0.24-beta1"))
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(setq ropemacs-loaded nil)
(defun load-ropemacs ()
    "Load pymacs and ropemacs"
    (interactive)
    (require 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    ;; Automatically save project python buffers before refactorings
    (setq ropemacs-confirm-saving 'nil)
    (setq ropemacs-loaded t)
    )

(defun load-ropemacs-if-not-loaded ()
  "Load pymacs and ropemacs as part of python mode hook"
  (if (not ropemacs-loaded) (load-ropemacs))
  )

(defun my-python-mode-hook()
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (set-variable 'indent-tabs-mode nil)
  (set-variable 'tab-width 4)
  (set-variable 'py-indent-offset 4)
  (lambda () (eldoc-mode 1))
  (local-set-key "\C-ch" 'pylookup-lookup)
  (load-ropemacs-if-not-loaded)
  (ropemacs-mode 1)
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)
