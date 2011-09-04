;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup for el-get
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working el-get.

(setq el-get-sources
      '(
	;; (:name quick-yes
	;;        :type http
	;;        :url "ftp://download.tuxfamily.org/user42/quick-yes.el")

	;; lua-mode 
	;; mo-git-blame
	;; offlineimap
	;; osc
	;; pycomplete+
	;; ropemacs
	;; slime
	;; twittering-mode
	;; wanderlust
	;; xml-parse
	auto-complete
	cedet
	cmake-mode 
	color-theme
	dired-single
	doxymacs
	el-get
	emacs-w3m
	erc-extras
	erc-highlight-nicknames
	filladapt
	flymake-point
	gnus-gravatar
	google-maps
	google-weather
	gravatar 
	haskell-mode
	highlight-parentheses
	icomplete+
	ido-hacks
	ioccur
	magit
	markdown-mode
	mingus
	nxhtml
	org-buffers
	org-mode
	pastebin
	processing-mode
	pylookup
	pymacs
	python
	rainbow-delimiters
	smex
	sml-modeline
	todochiku
	undo-tree
	workgroups
	yasnippet
	))

(add-to-list 'load-path (expand-file-name (concat qdot/emacs-autoinst-elisp-dir "el-get")))
(setq el-get-dir qdot/emacs-autoinst-elisp-dir)
(setq el-get-verbose t)

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path (expand-file-name (concat qdot/emacs-autoinst-elisp-dir "el-get/recipes/")))
(setq el-get-status-file (expand-file-name (concat qdot/emacs-conf-dir "elget-status.el")))

(loop for src in el-get-sources
      do (add-to-list 'load-path (expand-file-name (concat qdot/emacs-autoinst-elisp-dir (el-get-source-name src)))))

(el-get)
