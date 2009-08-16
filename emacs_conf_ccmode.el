(setq auto-mode-alist
      (append
	   '(
		 ("\\.h$"    . c++-mode)
		 ("\\.H$"    . c++-mode)
		 ) auto-mode-alist)
	  )

(defun my-cc-code-mode-hook ()
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (c-add-style "my-cc-code-style" '("bsd" (c-basic-offset . 4)))
  ;;(c-add-style "ll-code-style" '("bsd"))
  (setq indent-tabs-mode t)
  (setq-default tab-width 4)
  (c-set-style "my-cc-code-style")
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (local-set-key [(control tab)] 'semantic-complete-self-insert)
)
(add-hook 'c-mode-hook 'my-cc-code-mode-hook)
(add-hook 'c++-mode-hook 'my-cc-code-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hooks for recompilation and error maneuvering
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-recompile ()
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

(defun my-next-error ()
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(defun my-previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )
;; (global-set-key (kbd "C-n") 'my-next-error)
;; (global-set-key (kbd "C-p") 'my-previous-error)

(global-set-key [f5] 'my-recompile)

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
		)
      )

(setq ff-search-directories '(
							  "."
							  "../../src/*"
							  "../../../src/*"
							  "../../../src/geotools/*"
							  "../../include/*"
							  "../../include/geotools/*"
							  "../../../include/geotools/*"
							  "../../../include/*"
							  "../include/geotools/*"
							  "../include/*"
							  "../src/*"
							  "$HOME/build/*"
							  "$HOME/git-projects/*"
							  "/usr/*/include/*"
							  )
	  )
