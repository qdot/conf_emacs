;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; load file modes for programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; doxymacs mode for editing doxygen
(add-hook 'c-mode-common-hook 'doxymacs-mode)

;; Set defaults we expect
(setq-default c-basic-offset 2)
(setq-default py-indent-offset 2)

;; gdb/gud
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gud-chdir-before-run nil)
(setq gud-tooltip-mode t)

;; turn on linum mode for programming
(setq linum-format "%4d")

;; I don't always show parens, but when I do...
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(defun qdot/programming-mode-hook ()
  (linum-mode 1)
	;; fci-mode violently broken at the moment (24.3 HEAD)
	;;  (fci-mode 1)
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode 1)
  (set-fill-column 80)
  (setq show-trailing-whitespace t)
  (set-face-background 'show-paren-match-face "#222")
  (set-face-attribute 'show-paren-match-face nil
		      :weight 'bold :underline nil :overline nil :slant 'normal))

(add-hook 'emacs-lisp-mode-hook 'qdot/programming-mode-hook)
(add-hook 'cmake-mode-hook 'qdot/programming-mode-hook)
(add-hook 'c-mode-common-hook 'qdot/programming-mode-hook)
(add-hook 'haskell-mode-hook 'qdot/programming-mode-hook)
(add-hook 'java-mode-hook 'qdot/programming-mode-hook)
(add-hook 'js2-mode-hook 'qdot/programming-mode-hook)
(add-hook 'nxml-mode-hook 'qdot/programming-mode-hook)
(add-hook 'xml-mode-hook 'qdot/programming-mode-hook)
(add-hook 'nxhtml-mode-hook 'qdot/programming-mode-hook)
(add-hook 'python-mode-hook 'qdot/programming-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flymake with mode fixes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flymake)
(defun qdot/flymake-off-hook ()
  (flymake-mode 0))
(add-hook 'nxml-mode-hook 'qdot/flymake-off-hook)
(add-hook 'c++-mode-hook 'qdot/flymake-off-hook)
(add-hook 'c-mode-hook 'qdot/flymake-off-hook)
(add-hook 'xml-mode-hook 'qdot/flymake-off-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; haskell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'haskell-mode)
;;(require 'inf-haskell)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'font-lock-mode)
;;(setq haskell-font-lock-symbols t)

;; (defun flymake-Haskell-init ()
;;   (flymake-simple-make-init-impl
;;    'flymake-create-temp-with-folder-structure nil nil
;;    (file-name-nondirectory buffer-file-name)
;;    'flymake-get-Haskell-cmdline))

;; (defun flymake-get-Haskell-cmdline (source base-dir)
;;   (list "flycheck_haskell.pl"
;; 	(list source base-dir)))

;; (push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)
;; (push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)
;; (push
;;  '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
;;    1 2 3 4) flymake-err-line-patterns)

;; (add-hook
;;  'haskell-mode-hook
;;  '(lambda ()
;;     (if (not (null buffer-file-name)) (flymake-mode))))

;; Taken from http://www.credmp.org/index.php/2007/07/20/on-the-fly-syntax-checking-java-in-emacs/

;; (defun qdot/flymake-display-err-minibuf () 
;;   "Displays the error/warning for the current line in the minibuffer"
;;   (interactive)
;;   (let* ((line-no             (flymake-current-line-no))
;; 	 (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
;; 	 (count               (length line-err-info-list))
;; 	 )
;;     (while (> count 0)
;;       (when line-err-info-list
;; 	(let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
;; 	       (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
;; 	       (text (flymake-ler-text (nth (1- count) line-err-info-list)))
;; 	       (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
;; 	  (message "[%s] %s" line text)
;; 	  )
;; 	)
;;       (setq count (1- count)))))

;; (add-hook
;;  'haskell-mode-hook
;;  '(lambda ()
;;     (define-key haskell-mode-map "\C-cd"
;;       'qdot/flymake-display-err-minibuf)))

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

(defun qdot/cc-mode-hook ()
  (doxymacs-font-lock)
  (local-set-key (kbd "\C-m") 'newline-and-indent)
  (c-add-style "qdot/cc-code-style" '("bsd" (c-basic-offset . 2)))
  (setq indent-tabs-mode nil)
  (setq-default tab-width 2)
  (c-set-style "qdot/cc-code-style")
  (c-set-offset 'innamespace 0)
  (local-set-key [(control tab)] 'semantic-complete-self-insert)
  (subword-mode 1))

(add-hook 'c-mode-common-hook 'qdot/cc-mode-hook)

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
    (when (get-buffer "*compilation*")	; If old compile window exists
      (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
      (kill-buffer "*compilation*")) ; and kill the buffers
    (call-interactively 'compile)
    (enlarge-window 30)))

(defun qdot/next-error ()
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)))

(defun qdot/previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)))

;; (global-set-key (kbd "C-n") 'qdot/next-error)
;; (global-set-key (kbd "C-p") 'qdot/previous-error)

(global-set-key [f5] 'qdot/recompile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CEDET settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs freaks out if this isn't set.
(setq warning-suppress-types nil) 

;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)

(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/clang)
(require 'semantic/ia)
(require 'semantic/decorate/include)
(require 'semantic/lex-spp)

;; need to add CEDET contrib to bring in eassist
(add-to-list 'load-path (expand-file-name 
			 (concat
			  qdot/emacs-autoinst-elisp-dir "cedet/contrib")))

(require 'eassist)
;; (global-ede-mode 1)

(setq-default semanticdb-default-save-directory "~/.emacs_meta/semanticdb/"
	      semanticdb-default-system-save-directory "~/.emacs_meta/semanticdb/")

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
  (local-set-key (kbd "C-c o") 'eassist-switch-h-cpp)
  (local-set-key (kbd "C-c C-r") 'semantic-symref))
(add-hook 'c-mode-common-hook 'qdot/c-mode-cedet-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flymake + pyflakes syntax checking for python
;; http://plope.com/Members/chrism/flymake-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO This freaks out if pyflakes isn't available on the system
(if macosx-p
    ;; Usually using homebrew on OS X
    (when (file-exists-p "/opt/homebrew/Cellar/python/2.7/bin/pyflakes")
      (setq flyflakes-pyflakes-command 
	    '("/opt/homebrew/Cellar/python/2.7/bin/pyflakes"))
      (require 'flyflakes))
  (when (file-exists-p "/usr/local/bin/pyflakes")
    (setq flyflakes-pyflakes-command '("/usr/local/bin/pyflakes"))
    (require 'flyflakes)))

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
    ))

(defun qdot/python-mode-hook()
  (lambda () (eldoc-mode 1))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq py-indent-offset 4)
  (setq python-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil)
  (qdot/load-pymacs)
  (auto-complete-mode nil)
  (subword-mode 1)
  (set (make-local-variable 'ac-sources)
       (setq ac-sources (append '(ac-source-ropemacs ac-source-yasnippet) ac-sources)))
  (set (make-local-variable 'ac-find-function) 'ac-python-find))

;; (add-hook 'python-mode-hook 'qdot/ac-config-python)
(add-hook 'python-mode-hook 'qdot/python-mode-hook)

;; Guess tabs/spaces for python mode indentation
;; (add-hook 'python-mode-hook guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook (lambda ()
			      (when indent-tabs-mode
				(guess-style-guess-tab-width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Javascript settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq js-indent-level 2)
(setq
 js2-auto-indent-p t
 js2-basic-offset 2
 js2-enter-indents-newline t
 js2-indent-on-enter-key t)

;; (add-hook 'compilation-mode-hook
;; 					(lambda ()
;; 						(setq comint-buffer-maximum-size 10240)))
;; ;; (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)))

;; Fix for .js files that have Java set as the mode (I'm looking at
;; you, mozilla-central)
(add-hook 'java-mode-hook
	  (lambda ()
	    (when (string-match "\\.js\\'" buffer-file-name)
	      (js2-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; gdb settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off non-stop by default. All or nothing, damnit.
(setq gdb-non-stop-setting nil)

(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output 'first-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lisp/elisp settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eldoc mode for showing function calls in mode line
(setq eldoc-idle-delay 0)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; stealin' things from esk
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))))

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; TODO: look into parenface package
(defface esk-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses."
  :group 'starter-kit-faces)

(dolist (mode '(scheme emacs-lisp lisp))
    (when (> (display-color-cells) 8)
      (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                              '(("(\\|)" . 'esk-paren-face)))))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green1")
     (set-face-foreground 'magit-diff-del "red1")
     (set-face-background 'magit-diff-add "#004400")
     (set-face-background 'magit-diff-del "#440000")
		 (set-face-background 'magit-item-highlight "#1f2727")))
