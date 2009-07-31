;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up ido mode for command completion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;interactive-do mode
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

(setq ido-execute-command-cache nil)

(setq ido-create-new-buffer 'always)

;; Have ido complete M-x commands
;; Using icomplete+ for the moment (set up in emacs_conf_exts), as it's faster and a little easier to read

;; (defun ido-execute-command ()
;;   (interactive)
;;   (call-interactively
;;    (intern
;;     (ido-completing-read
;;      "M-x "
;;      (progn
;;        (unless ido-execute-command-cache
;; 		 (mapatoms (lambda (s)
;; 					 (when (commandp s)
;; 					   (setq ido-execute-command-cache
;; 							 (cons (format "%S" s) ido-execute-command-cache))))))
;;        ido-execute-command-cache)))))

;; (add-hook 'ido-setup-hook
;; 		  (lambda ()
;; 			(setq ido-enable-flex-matching t)
;; 			(global-set-key "\M-x" 'ido-execute-command)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
