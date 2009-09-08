;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; fix backspace key
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-h" 'delete-backward-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add c-c c-x or c-m c-x as meta triggers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-M-g") 'goto-line)
(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-c s") 'shell-current-directory)
(global-set-key (kbd "M-g s") 'magit-status)
;(global-set-key [(M-return)] 'toggle-max-window)
;;(define-key global-map " C-" 'previous-buffer)
;;(define-key global-map [C-}] 'next-buffer)

(defalias 'rs 'replace-string)
(defalias 'xp 'replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'is 'ispell)
(defalias 'll 'visual-line-mode)
