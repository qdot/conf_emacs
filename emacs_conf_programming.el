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

(autoload 'lua-mode "lua-mode" 
  "Lua editing mode." t)

(load-library "flymake_cursor.el")

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
         ("\\.lua$" . lua-mode)
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
                                     xml-mode
                                     nxml-mode
                                     python-mode
				     haskell-mode
				     emacs-lisp-mode
				     lisp-mode 
                                     scheme-mode))

(defun my-trailing-whitespace-hook ()
  (when (member major-mode my-trailing-whitespace-modes)
    (delete-trailing-whitespace)))
;; Changing too many files and making my commits messy
;; (add-hook 'before-save-hook 'my-trailing-whitespace-hook)
(when (file-exists-p "~/.emacs_files/elisp_local/magit")
  (add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_local/magit/"))
  (require 'magit)
  )

;; eldoc mode for showing function calls in mode line
(setq eldoc-idle-delay 0)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; turn on linum mode for programming
(setq linum-format "%d ")

(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'cmake-mode-hook 'linum-mode)
(add-hook 'c++-mode-hook 'linum-mode)
(add-hook 'c-mode-hook 'linum-mode)

