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


;; "http://www.skamphausen.de/cgi-bin/ska/download/active-menu.el"
;; "http://sensorflo-emacs.googlecode.com/svn/trunk/adoc-mode/adoc-mode.el"
;; "http://bitbucket.org/kcfelix/chuck-mode/raw/d713e29c4c25/chuck-mode.el"
;; "http://www.cmake.org/CMakeDocs/cmake-mode.el"
;; "http://xahlee.org/emacs/command-frequency.el"
;; "http://www.emacswiki.org/emacs/download/dired-single.el"
;; "http://www.wonderworks.com/download/filladapt.el"
;; "http://www.emacswiki.org/emacs/download/frame-fns.el"
;; "http://www.emacswiki.org/emacs/download/frame-cmds.el"
;; "http://www.emacswiki.org/emacs/download/framemove.el"
;; "http://www.emacswiki.org/emacs/download/icomplete+.el"
;; "http://jblevins.org/projects/markdown-mode/markdown-mode.el"
;; "http://www.bunkus.org/cgi-bin/gitweb.cgi?p=mo-git-blame.git;a=blob_plain;f=mo-git-blame.el;hb=HEAD"
;; "http://files.taesoo.org/git-emacs/git-modeline.el"
;; "http://luaforge.net/plugins/scmcvs/cvsweb.php/lua-mode/lua-mode.el?rev=HEAD;content-type=text/plain;cvsroot=lua-mode;only_with_tag=HEAD"
;; "http://cvs.savannah.gnu.org/viewvc/*checkout*/circe/lui.el?revision=HEAD&root=circe"
;; "http://cvs.savannah.gnu.org/viewvc/*checkout*/circe/lui-format.el?revision=HEAD&root=circe"
;; "http://www.emacswiki.org/emacs/download/mudel.el"
;; "http://delysid.org/emacs/osc.el"
;; "http://mumble.net/~campbell/emacs/paredit.el"
;; "http://github.com/omouse/processing-emacs/raw/master/processing-mode.el"
;; "ftp://download.tuxfamily.org/user42/quick-yes.el"
;; "http://www.gentei.org/~yuuji/software/revive.el"
;; "http://bazaar.launchpad.net/~nxhtml/nxhtml/main/download/head%3A/smlmodeline.el-20100318165023-n7kkswg6dlq8l6b3-1/sml-modeline.el"
;; "http://www.emacswiki.org/emacs/download/todochiku.el"
;; "http://www.gentei.org/~yuuji/software/windows.el"

(setq el-get-sources
      '(
	;; (:name quick-yes
	;;        :type http
	;;        :url "ftp://download.tuxfamily.org/user42/quick-yes.el")
	;; (:name elscreen
	;;        :type http-tar
	;;        :options ("xzf")
	;;        :url "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-1.4.6.tar.gz")
	;; (:name elscreen-buffer-list
	;;        :type emacswiki)
	;; (:name elscreen-color-theme
	;;        :type http-tar
	;;        :options ("xzf")
	;;        :url "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-color-theme-0.0.0.tar.gz")
	;; (:name elscreen-speedbar
	;;        :type http-tar
	;;        :options ("xzf")
	;;        :url "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-speedbar-0.0.0.tar.gz")
	;; (:name elscreen-w3m
	;;        :type http-tar
	;;        :options ("xzf")
	;;        :url "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-w3m-0.2.2.tar.gz")
	;; (:name elscreen-server
	;;        :type http-tar
	;;        :options ("xzf")
	;;        :url "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-server-0.2.0.tar.gz")
	;; (:name elscreen-dired
	;;        :type http-tar
	;;        :options ("xzf")
	;;        :url "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-dired-0.1.0.tar.gz")
	;; (:name elscreen-wl
	;;        :type http-tar
	;;        :options ("xzf")
	;;        :url "ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-wl-0.8.0.tar.gz")
	;; (:name frame-fns
	;;        :type emacswiki)
	;; (:name pycomplete+
	;;        :type emacswiki)
	;;python-mode
	icomplete+
	xml-parse
	dired-single
	active-menu
	adoc-mode
	erc-extras
	cmake-mode
	chuck-mode
	command-frequency
	gravatar
	gnus-gravatar
	mo-git-blame
	lua-mode
	lui
	lui-format
	osc
	paredit
	revive
	sml-modeline
	windows-mode
	workgroups
	processing-mode
	color-theme
	bbdb
	auto-complete
	emacs-w3m	
	erc-highlight-nicknames
	;;gist
	google-maps
	google-weather
	haskell-mode
	highlight-parentheses
	ido-hacks
	ioccur
	lisppaste
	magit
	nxhtml
	offlineimap
	org-mode
	pastebin
	slime
	todochiku
	undo-tree
	yasnippet
	filladapt
	markdown-mode
	pylookup
	mingus
	))

(add-to-list 'load-path (expand-file-name (concat emacs-repo-autoinst-elisp-dir "el-get")))
(require 'el-get)
(setq el-get-dir emacs-repo-autoinst-elisp-dir)
(add-to-list 'el-get-recipe-path (expand-file-name (concat emacs-repo-autoinst-elisp-dir "el-get/recipes/")))
(setq el-get-status-file (expand-file-name (concat emacs-repo-autoinst-elisp-dir "elget-status.el")))

(loop for src in el-get-sources
      do (add-to-list 'load-path (expand-file-name (concat emacs-repo-autoinst-elisp-dir (el-get-source-name src)))))

(if macosx-p
    (custom-set-variables
     '(magit-git-executable "/usr/local/git/bin/git")
     )
  )

(el-get)