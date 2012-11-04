;; Set the prefix key to tilde, what I normally use in screen
(setq wg-prefix-key "`")
;; Turn off animations
(setq wg-morph-on nil)
;; Turn off saving on exit
(setq wg-emacs-exit-save-behavior nil)
;; Turn off reloading of workgroup file lists
(setq wg-restore-associated-buffers nil)

(setq wg-switch-to-first-workgroup-on-find-session-file nil)

;; Due to some buffers having issues when reloading (erc), uids start
;; to mismatch which massively trips up workgroups. This is function
;; redefinition allows us to set the action when a mismatch happens.

(defvar wg-error-on-uid-mismatch t
  "Whether or not we should throw an error when buffer uids
mismatch, or just throw a message and reset them to what we think
they should be.")

(setq wg-error-on-uid-mismatch nil)

(defun wg-set-buffer-uid-or-error (uid &optional buffer)
  "Set BUFFER's buffer local value of `wg-buffer-uid' to UID.
If BUFFER already has a buffer local value of `wg-buffer-uid',
and it's not equal to UID, error."
  (if wg-buffer-uid
      (if (string= wg-buffer-uid uid) uid
	(if wg-error-on-uid-mismatch
	    (error "uids don't match %S and %S for %S" 
		   uid wg-buffer-uid
		   (if buffer (buffer-name buffer) 
		     (buffer-name (current-buffer))))
	  (setq wg-buffer-uid uid)
	  (message "uids don't match %S and %S for %S" 
		   uid wg-buffer-uid
		   (if buffer (buffer-name buffer) 
		     (buffer-name (current-buffer))))))
    (setq wg-buffer-uid uid)))

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

(defun qdot/wg-buffer-list-filter-associated-not-irc (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST in `erc-mode'."
  (qdot/wg-filter-buffer-list-by-not-major-mode
   'erc-mode (wg-buffer-list-filter-associated workgroup buffer-list)))


(defun qdot/wg-buffer-list-filter-erc-channel (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST in `erc-mode'."
  (wg-filter-buffer-list-by-regexp 
   "^#" (wg-filter-buffer-list-by-major-mode 'erc-mode buffer-list)))

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

(add-to-list
 'wg-buffer-list-filter-definitions
 '(qdot/associated-not-irc "qdot/associated-not-irc" 
			   qdot/wg-buffer-list-filter-associated-not-irc))

(defun qdot/wg-set-buffer-lists ()
  (wg-set-workgroup-parameter (wg-get-workgroup "work") 
  			      'wg-buffer-list-filter-order-alist 
  			      '((default qdot/associated-not-irc qdot/not-irc all)))
  (wg-set-workgroup-parameter (wg-get-workgroup "scratch")
			      'wg-buffer-list-filter-order-alist 
			      '((qdot/not-irc all)))
  (wg-set-workgroup-parameter (wg-get-workgroup "erc") 
			      'wg-buffer-list-filter-order-alist 
			      '((default qdot/erc-irc all)))
  (wg-set-workgroup-parameter (wg-get-workgroup "bitlbee") 
			      'wg-buffer-list-filter-order-alist 
			      '((default qdot/erc-query all))))

(defun qdot/wg-load ()
  (interactive)
  (wg-find-session-file (concat qdot/emacs-conf-dir "workgroups/linux-wg.el"))
  (qdot/wg-set-buffer-lists))

;; (wg-filter-buffer-list-by-major-mode 'erc-mode (buffer-list))
;; (wg-filter-buffer-list-by-not-major-mode 'erc-mode (buffer-list))

;; (defvar qdot/reallocate-query-buffer-trigger t
;; 	"Used to know when to trigger a query buffer reallocation on workgroup change")

;; (defun qdot/trigger-reallocate-query-buffers (proc parsed)
;; 	(setq qdot/reallocate-query-buffer-trigger t))

;; (add-hook 'erc-server-PRIVMSG-functions 'qdot/trigger-reallocate-query-buffers)

;; ;; Auto-place IM windows when switching to bitlbee workgroup
;; (add-hook 'wg-switch-to-workgroup-hook 
;; 	  (lambda () 
;;  	    (when (and qdot/reallocate-query-buffer-trigger
;; 								 (eq (wg-get-workgroup "bitlbee") (wg-current-workgroup)))
;; 				(qdot/bitlbee-resume-layout)
;; 				(setq qdot/reallocate-query-buffer-trigger nil))
;; 	    (when (eq (wg-get-workgroup "erc") (wg-current-workgroup))
;; 	      (qdot/erc-set-fill-columns))
;; 	    (when (eq (wg-get-workgroup "mozilla") (wg-current-workgroup))
;; 	      (qdot/erc-set-fill-columns))))


			    
