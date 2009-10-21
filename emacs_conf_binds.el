;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key triggers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-c s") 'shell-current-directory)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c C-s") 'sudo-edit-current-file)
(global-set-key (kbd "C-c C-r") 'reload-file)
(global-set-key (kbd "C-M-g") 'goto-line)
(global-set-key (kbd "M-g s") 'magit-status)
;;(define-key global-map " C-" 'previous-buffer)
;;(define-key global-map [C-}] 'next-buffer)

(defalias 'rs 'replace-string)
(defalias 'xp 'replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'is 'ispell)
(defalias 'll 'visual-line-mode)
