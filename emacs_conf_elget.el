;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup for el-get
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working el-get.

(defun qdot/load-el-get()
  (interactive)
  (let* ((el-get-dir        emacs-repo-autoinst-elisp-dir)
	 (dummy             (unless (file-directory-p el-get-dir)
			      (make-directory el-get-dir t)))
	 (package           "el-get")
	 (bname             "*el-get bootstrap*") ; both process and buffer name
	 (pdir              (concat (file-name-as-directory el-get-dir) package))
	 (git               (or (executable-find "/usr/local/git/bin/git") (error "Unable to find `git'")))
	 (url               "git://github.com/dimitri/el-get.git")
	 (el-get-sources    `((:name ,package :type "git" :url ,url :features el-get :compile "el-get.el")))
	 (default-directory el-get-dir)
	 (process-connection-type nil) ; pipe, no pty (--no-progress)
	 (clone             (start-process bname bname git "--no-pager" "clone" "-v" url package)))
    (set-window-buffer (selected-window) (process-buffer clone))
    (set-process-sentinel
     clone
     `(lambda (proc change)
	(when (eq (process-status proc) 'exit)
	  (setq default-directory (file-name-as-directory ,pdir))
	  (setq el-get-sources ',el-get-sources)
	  (load (concat (file-name-as-directory ,pdir) ,package ".el"))
	  (el-get-init "el-get")
	  (with-current-buffer (process-buffer proc)
	    (goto-char (point-max))
	    (insert "\nCongrats, el-get is installed and ready to serve!")))))))


(setq el-get-sources
      '(
	;; (:name quick-yes
	;;        :type http
	;;        :url "ftp://download.tuxfamily.org/user42/quick-yes.el")
	python
	icomplete+
	dired-single
	active-menu
	adoc-mode
	cmake-mode 
	gravatar 
	gnus-gravatar
	flymake-point
	sml-modeline
	workgroups
	color-theme
	auto-complete
	erc-highlight-nicknames
	google-maps
	google-weather
	haskell-mode
	highlight-parentheses
	ido-hacks
	ioccur
	magit
	nxhtml
	todochiku
	undo-tree
	yasnippet
	filladapt
	markdown-mode
	pylookup
	mingus
	doxymacs
	pymacs
	org-mode
	org-buffers
	cedet
	wanderlust
	smex
	rainbow-delimiters
	;; pycomplete+
	;; ropemacs
	;; el-get
	;; xml-parse
	;; frame-fns
	;; frame-cmds
	;; erc-extras
	;; mo-git-blame
	;; lua-mode 
	;; circe
	;; osc
	;; paredit
	;; processing-mode
	;; bbdb
	;; emacs-w3m	
	;; gist
	;; lisppaste
	;; offlineimap
	;; pastebin
	;; slime
	;; ibuffer-git
	;; twittering-mode
	;; deldo
	;; jerkcity
	))

(add-to-list 'load-path (expand-file-name (concat emacs-repo-autoinst-elisp-dir "el-get")))
(setq el-get-dir emacs-repo-autoinst-elisp-dir)
(require 'el-get)
(add-to-list 'el-get-recipe-path (expand-file-name (concat emacs-repo-autoinst-elisp-dir "el-get/recipes/")))
(setq el-get-status-file (expand-file-name (concat emacs-repo-autoinst-elisp-dir "elget-status.el")))

(loop for src in el-get-sources
      do (add-to-list 'load-path (expand-file-name (concat emacs-repo-autoinst-elisp-dir (el-get-source-name src)))))

(el-get)
