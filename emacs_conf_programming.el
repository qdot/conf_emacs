;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; load file modes for programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; doxymacs mode for editing doxygen
(add-hook 'c-mode-common-hook 'doxymacs-mode)

(defun qdot/doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'qdot/doxymacs-font-lock-hook)

(require 'haskell-mode)
(require 'inf-haskell)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(setq haskell-font-lock-symbols t)

;; gdb/gud
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gud-chdir-before-run nil)
(setq gud-tooltip-mode t)

;; gtags
(autoload 'gtags-mode "gtags" "" t)

;; eldoc mode for showing function calls in mode line
(setq eldoc-idle-delay 0)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; turn on linum mode for programming
(setq linum-format "%4d")

(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'cmake-mode-hook 'linum-mode)
(add-hook 'c++-mode-hook 'linum-mode)
(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'haskell-mode-hook 'linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flymake with mode fixes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flymake)
(defun qdot/flymake-off-hook ()
  (flymake-mode 0)
)
(add-hook 'haskell-mode-hook 'qdot/flymake-off-hook)
(add-hook 'nxml-mode-hook 'qdot/flymake-off-hook)
(add-hook 'c++-mode-hook 'qdot/flymake-off-hook)
(add-hook 'c-mode-hook 'qdot/flymake-off-hook)
(add-hook 'xml-mode-hook 'qdot/flymake-off-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; smerge mode for looking at inline conflicts
;; http://atomized.org/2010/06/resolving-merge-conflicts-the-easy-way-with-smerge-kmacro/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Super simple generic mode for nsis editing
;; God forbid I have to do that often
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode 'nsis-generic-mode
  nil ;;'(";")
  '("Section" "SectionEnd" "Function" "FunctionEnd" "Call" "Goto")
  '(("!\\([A-Za-z]+\\)" (1 'font-lock-builtin-face))
    ("$[({]?\\([A-Za-z0-9_]+\\)[)}]?" (1 'font-lock-variable-name-face))
    )
  (list "\\.\\(nsi\\|nsh\\)$")    
  nil
  "Generic mode for nsis files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cc-mode hook
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun qdot/cc-code-mode-hook ()
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (c-add-style "qdot/cc-code-style" '("bsd" (c-basic-offset . 4)))
  (setq indent-tabs-mode t)
  (setq-default tab-width 4)
  (c-set-style "qdot/cc-code-style")
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (local-set-key [(control tab)] 'semantic-complete-self-insert)
  )

(add-hook 'c-mode-hook 'qdot/cc-code-mode-hook)
(add-hook 'c++-mode-hook 'qdot/cc-code-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hooks for recompilation and error maneuvering
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)

(defun qdot/recompile ()
  "Run compile and resize the compile window closing the old one if necessary"
  (interactive)
  (progn
    (if (get-buffer "*compilation*")	; If old compile window exists
        (progn
          (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
          (kill-buffer "*compilation*") ; and kill the buffers
          )
      )
    (call-interactively 'compile)
    (enlarge-window 30)
    )
  )

(defun qdot/next-error ()
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(defun qdot/previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )
;; (global-set-key (kbd "C-n") 'qdot/next-error)
;; (global-set-key (kbd "C-p") 'qdot/previous-error)

(global-set-key [f5] 'qdot/recompile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use c-c c-o to switch between header and implementation files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(setq cc-other-file-alist
      '(
        ("\\.cpp$" (".h" ".hpp" ".hh"))
        ("\\.cxx$" (".hpp" ".h" ".hh"))
        ("\\.h$" (".c" ".cpp" ".cc" ".cxx"))
        ("\\.c$" (".hpp" ".h" ".hh" ".hxx"))
        ("\\.hpp$" (".cpp" ".c" ".cc" ".cxx"))
        ("\\.hh$" (".cc"))
        ("\\.cc$" (".hh"))
        ))

(setq ff-search-directories '(
                              "."
                              "$HOME/git-projects/*"
                              "/usr/*/include/*"
                              ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CEDET settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs_files/elisp_auto/cedet/common/cedet.el")

;; Emacs freaks out if this isn't set.
(setq warning-suppress-types nil) 

(setq-default semanticdb-default-save-directory "~/.emacs_meta/semanticdb/"
	      semanticdb-default-system-save-directory "~/.emacs_meta/semanticdb/")

(setq semantic-load-turn-useful-things-on 1)
;;(semantic-load-enable-code-helpers)
;;(semantic-load-enable-gaudy-code-helpers)
(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

(custom-set-variables
 '(semantic-idle-scheduler-idle-time 3))

(require 'semantic-ia)
(require 'semantic-gcc)
(require 'semantic-decorate-include)

(setq-mode-local c-mode
		 semanticdb-find-default-throttle
		 '(project unloaded system recursive))
(setq-mode-local c++-mode
		 semanticdb-find-default-throttle
		 '(project unloaded system recursive))

;; (defun qdot/cc-mode-cedet-idle-hook()
;;   (semantic-idle-scheduler-mode 1)
;;   (semantic-idle-completions-mode 1)
;;   (semantic-idle-summary-mode 1))

;; (add-hook 'c-mode-hook 'qdot/cc-mode-cedet-idle-hook)
;; (add-hook 'c++-mode-hook 'qdot/cc-mode-cedet-idle-hook)

(require 'semantic-lex-spp)

;; hooks, specific for semantic
(defun qdot/semantic-hook ()
  ;; (semantic-tag-folding-mode 1)
  (imenu-add-to-menubar "TAGS")
  )
(add-hook 'semantic-init-hooks 'qdot/semantic-hook)
(global-semantic-tag-folding-mode 1)

;; gnu global support
;; (require 'semanticdb-global)
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)

(defun qdot/cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'qdot/cedet-hook)
(add-hook 'lisp-mode-hook 'qdot/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'qdot/cedet-hook)

(defun qdot/c-mode-cedet-hook ()

  ;; Uncomment these for full intellisense typeahead
  ;; Currently very slow
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'qdot/c-mode-cedet-hook)

(setq-mode-local c-mode semanticdb-find-default-throttle
		 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
		 '(project unloaded system recursive))

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

(setq qdot/pymacs-loaded nil)

(defun qdot/load-pymacs()
  (interactive)
  (unless qdot/pymacs-loaded
    (autoload 'pymacs-apply "pymacs")
    (autoload 'pymacs-call "pymacs")
    (autoload 'pymacs-eval "pymacs" nil t)
    (autoload 'pymacs-exec "pymacs" nil t)
    (autoload 'pymacs-load "pymacs" nil t)
    (pymacs-load "ropemacs" "rope-")
    (setq ropemacs-enable-autoimport t)
    (ac-ropemacs-require)
    (setq qdot/pymacs-loaded t)
    )
)



(defun qdot/python-mode-hook()
  (font-lock-mode 1)
  (linum-mode 1)
  (font-lock-fontify-buffer)
  (set-variable 'indent-tabs-mode nil)
  (set-variable 'tab-width 4)
  (set-variable 'py-indent-offset 4)
  (lambda () (eldoc-mode 1))
  (local-set-key "\C-ch" 'pylookup-lookup)
  (qdot/load-pymacs)
  (auto-complete-mode nil)
  (set (make-local-variable 'ac-sources)
       (setq ac-sources (append '(ac-source-ropemacs) ac-sources))
       )
  (set (make-local-variable 'ac-find-function) 'ac-python-find)
  )

;; (add-hook 'python-mode-hook 'qdot/ac-config-python)
(add-hook 'python-mode-hook 'qdot/python-mode-hook)
(remove-hook 'python-mode-hook 'wisent-python-default-setup)

