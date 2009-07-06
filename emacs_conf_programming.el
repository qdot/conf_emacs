;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; load file modes for programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; doxymacs mode for editing doxygen
(require 'doxymacs)
(add-hook 'c-mode-common-hook'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;add git hooks
;;(add-to-list 'vc-handled-backends 'Git)
;;(require 'git-emacs)

(autoload 'wikipedia-mode "wikipedia-mode"
  "Major mode for editing documents in Wikipedia markup." t)

(autoload 'php-mode "php-mode"
  "PHP editing mode" t)

(autoload 'cmake-mode "cmake-mode"
  "CMakeLists editing mode" t)

(autoload 'json "json"
  "JSON creation" t)

(autoload 'js2-mode "js2"
  "JS2 Javascript Editing" t)

;; file extension mode recognition
(setq auto-mode-alist
      (append
       '(
		 ("\\.php$"  . php-mode)
		 ("\\.py$"   . python-mode)
		 ("\\.sql$"  . sql-mode)
		 ("\\.mode$" . cmake-mode)
		 ("\\.php$" . php-mode)
		 ("\\.wiki$" . wikipedia-mode)
		 ("\\.json$" . python-mode)
		 ("\\.cmake$" . cmake-mode)
		 ("\\.asciidoc$" . asciidoc-mode)
		 ("CMakeLists\\.txt\\'" . cmake-mode)
		 ("ChangeLog\\.txt\\'" . change-log-mode)
		 ) auto-mode-alist))

;; gdb/gud
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gud-chdir-before-run nil)
(setq gud-tooltip-mode t)

;; gtags
(autoload 'gtags-mode "gtags" "" t)

;; clean trailing whitespaces automatically
(setq my-trailing-whitespace-modes '(c++-mode
				     c-mode
				     haskell-mode
				     emacs-lisp-mode
				     lisp-mode scheme-mode))
(defun my-trailing-whitespace-hook ()
  (when (member major-mode my-trailing-whitespace-modes)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-trailing-whitespace-hook)
(require 'magit)

