;; Autocomplete setup
(define-key ac-complete-mode-map [tab] 'ac-expand)
(ac-flyspell-workaround)
(add-to-list 'ac-modes 'org-mode)

;; (defvar ac-org-candidates nil)
;; (defvar ac-org-pattern nil)
;; (defun ac-org-construct-candidates ()
;;   "Pabbrev source for org."
;;   (let* ((end (point))
;; 	 (beg1 (save-excursion
;; 		 (skip-chars-backward (org-re "[:alnum:]_@"))
;; 		 (point)))
;; 	 (beg (save-excursion
;; 		(skip-chars-backward "a-zA-Z0-9_:$")
;; 		(point)))
;; 	 (confirm (lambda (x) (stringp (car x))))
;; 	 (searchhead (equal (char-before beg) ?*))
;; 	 (struct
;; 	  (when (and (member (char-before beg1) '(?. ?<))
;; 		     (setq a (assoc (buffer-substring beg1 (point))
;; 				    org-structure-template-alist)))
;; 	    (org-complete-expand-structure-template (1- beg1) a)
;; 	    (throw 'exit t)))
;; 	 (tag (and (equal (char-before beg1) ?:)
;; 		   (equal (char-after (point-at-bol)) ?*)))
;; 	 (prop (and (equal (char-before beg1) ?:)
;; 		    (not (equal (char-after (point-at-bol)) ?*))))
;; 	 (texp (equal (char-before beg) ?\\))
;; 	 (link (equal (char-before beg) ?\[))
;; 	 (opt (equal (buffer-substring (max (point-at-bol) (- beg 2))
;; 				       beg)
;; 		     "#+"))
;; 	 (startup (string-match "^#\\+STARTUP:.*"
;; 				(buffer-substring (point-at-bol) (point))))
;; 	 (completion-ignore-case opt)
;; 	 (type nil)
;; 	 (tbl nil)
;; 	 (table (cond
;; 		 (opt
;; 		  (setq type :opt)
;; 		  (require 'org-exp)
;; 		  (append
;; 		   (mapcar
;; 		    (lambda (x)
;; 		      (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
;; 		      (cons (match-string 2 x) (match-string 1 x)))
;; 		    (org-split-string (org-get-current-options) "\n"))
;; 		   (mapcar 'list org-additional-option-like-keywords)))
;; 		 (startup
;; 		  (setq type :startup)
;; 		  org-startup-options)
;; 		 (link (append org-link-abbrev-alist-local
;; 			       org-link-abbrev-alist))
;; 		 (texp
;; 		  (setq type :tex)
;; 		  org-html-entities)
;; 		 ((string-match "\\`\\*+[ \t]+\\'"
;; 				(buffer-substring (point-at-bol) beg))
;; 		  (setq type :todo)
;; 		  (mapcar 'list org-todo-keywords-1))
;; 		 (searchhead
;; 		  (setq type :searchhead)
;; 		  (save-excursion
;; 		    (goto-char (point-min))
;; 		    (while (re-search-forward org-todo-line-regexp nil t)
;; 		      (push (list
;; 			     (org-make-org-heading-search-string
;; 			      (match-string 3) t))
;; 			    tbl)))
;; 		  tbl)
;; 		 (tag (setq type :tag beg beg1)
;; 		      (or org-tag-alist (org-get-buffer-tags)))
;; 		 (prop (setq type :prop beg beg1)
;; 		       (mapcar 'list (org-buffer-property-keys nil t t)))
;; 		 (t (progn
;; 		      (call-interactively org-completion-fallback-command)
;; 		      (throw 'exit nil))))))
;;     (setq ac-org-pattern (buffer-substring-no-properties beg end))
;;     table))

;; (defvar ac-source-org nil)
;; (setq ac-source-org
;;       `((sigil . "o")
;; 	(init . (lambda () (setq ac-org-candidates
;; 				 (condition-case nil
;; 				     (ac-org-construct-candidates)))))
;; 	(candidates . (lambda ()
;; 			(all-completions ac-target ac-org-candidates)))))


