;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; load file modes for programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; doxymacs mode for editing doxygen
(add-hook 'c-mode-common-hook'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

(autoload 'wikipedia-mode "wikipedia-mode"
  "Major mode for editing documents in Wikipedia markup." t)

(autoload 'json "json"
  "JSON creation" t)

(autoload 'js2-mode "js2"
  "JS2 Javascript Editing" t)

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

;; clean trailing whitespaces automatically
;; (setq my-trailing-whitespace-modes '(c++-mode
;;                                      c-mode
;;                                      xml-mode
;;                                      nxml-mode
;;                                      python-mode
;;                                      haskell-mode
;;                                      emacs-lisp-mode
;;                                      lisp-mode 
;;                                      scheme-mode))

;; (defun my-trailing-whitespace-hook ()
;;   (when (member major-mode my-trailing-whitespace-modes)
;;     (delete-trailing-whitespace)))
;; Changing too many files and making my commits messy
;; (add-hook 'before-save-hook 'my-trailing-whitespace-hook)

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
(defun my-flymake-off-hook ()
  (flymake-mode 0)
)
(add-hook 'haskell-mode-hook 'my-flymake-off-hook)
(add-hook 'nxml-mode-hook 'my-flymake-off-hook)
(add-hook 'c++-mode-hook 'my-flymake-off-hook)
(add-hook 'c-mode-hook 'my-flymake-off-hook)
(add-hook 'xml-mode-hook 'my-flymake-off-hook)

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

