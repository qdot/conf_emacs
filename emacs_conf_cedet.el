(load-file "~/.emacs_files/elisp_local/cedet/common/cedet.el")

(global-ede-mode t)

(setq-default semanticdb-default-save-directory "~/.emacs_meta/semanticdb/"
              semanticdb-default-system-save-directory "~/.emacs_meta/semanticdb/")

(global-srecode-minor-mode 1)            ; Enable template insertion menu

(setq semantic-load-turn-useful-things-on 1)
(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-idle-summary-mode 1)

(custom-set-variables
 '(semantic-idle-scheduler-idle-time 3))

(require 'semantic-ia)

(require 'semantic-gcc)

(setq-mode-local c-mode
				 semanticdb-find-default-throttle
				 '(project unloaded system recursive))
(setq-mode-local c++-mode
				 semanticdb-find-default-throttle
				 '(project unloaded system recursive))
(setq-mode-local java-mode
				 semanticdb-find-default-throttle
				 '(project unloaded system recursive)) 


(require 'semantic-lex-spp)

;; hooks, specific for semantic
(defun my-semantic-hook ()
;; (semantic-tag-folding-mode 1)
  (imenu-add-to-menubar "TAGS")
 )
(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; gnu global support
(require 'semanticdb-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(semantic-add-system-include "/usr/local/include" 'c++-mode)
(semantic-add-system-include "/usr/local/include/boost" 'c++-mode)
(semantic-add-system-include "/usr/local/include" 'c-mode)


(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
(add-hook 'c-mode-common-hook 'my-cedet-hook)

(defun my-ede-get-local-var (fname var)
  "fetch given variable var from :local-variables of project of file fname"
  (let* ((current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (when prj
      (let* ((ov (oref prj local-variables))
            (lst (assoc var ov)))
        (when lst
          (cdr lst))))))

(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)
(defun My-Compile ()
  "Saves all unsaved buffers, and runs 'compile'."
  (interactive)
  (save-some-buffers t)
  (compile (or (my-ede-get-local-var (buffer-file-name (current-buffer)) 'compile-command)
               compile-command)))
(global-set-key [f9] 'My-Compile)

(setq qt4-base-dir "/Library/Frameworks/QtCore.framework/Headers")
(setq qt4-gui-dir "/Library/Frameworks/QtGui.framework/Headers")
(setq qt4-opengl-dir "/Library/Frameworks/QtOpenGL.framework/Headers")
(semantic-add-system-include qt4-base-dir 'c++-mode)
(semantic-add-system-include qt4-gui-dir 'c++-mode)
(semantic-add-system-include qt4-opengl-dir 'c++-mode)
(add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "qconfig.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "qconfig-dist.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "qglobal.h"))
