;; Set the prefix key to tilde, what I normally use in screen
(setq wg-prefix-key "`")
;; Turn off animations
(setq wg-morph-on nil)
;; Turn off saving on exit
(setq wg-emacs-exit-save-behavior nil)

(workgroups-mode 1)

(defun qdot/wg-filter-buffer-list-by-not-major-mode (major-mode buffer-list)
  "Return only those buffers in BUFFER-LIST in major-mode MAJOR-MODE."
  (remove-if (lambda (mm) (eq mm major-mode))
			 buffer-list :key 'wg-buffer-major-mode))

(defun qdot/wg-filter-buffer-list-by-erc-query (server buffer-list)
  "Return only those buffers in BUFFER-LIST in major-mode MAJOR-MODE."
  (remove-if-not (lambda (buf) (erc-query-buffer-p (get-buffer buf)))
		 buffer-list :key 'buffer-name))

(defun qdot/wg-buffer-list-filter-not-irc (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST in `erc-mode'."
  (qdot/wg-filter-buffer-list-by-not-major-mode 'erc-mode buffer-list))

(defun qdot/wg-buffer-list-filter-erc-channel (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST in `erc-mode'."
  (wg-filter-buffer-list-by-regexp "^#"
				   (wg-filter-buffer-list-by-major-mode 'erc-mode buffer-list)))

(defun qdot/wg-buffer-list-filter-erc-query (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST in `erc-mode'."
  (qdot/wg-filter-buffer-list-by-erc-query 'erc-mode buffer-list))

(add-to-list
 'wg-buffer-list-filter-definitions
 '(qdot/erc-query "qdot/erc-query" qdot/wg-buffer-list-filter-erc-query))
(add-to-list
 'wg-buffer-list-filter-definitions
 '(qdot/erc-irc "qdot/erc-channel" qdot/wg-buffer-list-filter-erc-channel))
(add-to-list
 'wg-buffer-list-filter-definitions
 '(qdot/not-irc "qdot/not-irc" qdot/wg-buffer-list-filter-not-irc))

(wg-set-workgroup-parameter (wg-get-workgroup "erc") 'wg-buffer-list-filter-order-alist '((default qdot/erc-irc all)))
(wg-set-workgroup-parameter (wg-get-workgroup "work") 'wg-buffer-list-filter-order-alist '((default qdot/not-irc all)))
(wg-set-workgroup-parameter (wg-get-workgroup "bitlbee") 'wg-buffer-list-filter-order-alist '((default qdot/erc-query all)))

;; (wg-filter-buffer-list-by-major-mode 'erc-mode (buffer-list))
;; (wg-filter-buffer-list-by-not-major-mode 'erc-mode (buffer-list))

