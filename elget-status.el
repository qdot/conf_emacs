((ace-jump-mode status "installed" recipe
		(:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (adoc-mode status "installed" recipe
	    (:name adoc-mode :website "http://code.google.com/p/adoc-mode/" :description "A major-mode for editing AsciiDoc files in Emacs." :type http :url "http://sensorflo-emacs.googlecode.com/svn/trunk/adoc-mode/adoc-mode.el" :features "adoc-mode" :compile nil))
 (auto-complete status "installed" recipe
		(:name auto-complete :website "http://cx4a.org/software/auto-complete/" :description "The most intelligent auto-completion extension." :type github :pkgname "m2ym/auto-complete" :depends popup :load-path "." :post-init
		       (progn
			 (require 'auto-complete)
			 (add-to-list 'ac-dictionary-directories
				      (expand-file-name "dict"))
			 (require 'auto-complete-config)
			 (ac-config-default))))
 (auto-complete-css status "installed" recipe
		    (:name auto-complete-css :description "Auto-complete sources for CSS" :type http :url "http://www.cx4a.org/pub/auto-complete-css.el"))
 (auto-complete-emacs-lisp status "installed" recipe
			   (:name auto-complete-emacs-lisp :description "Auto-complete sources for emacs lisp" :type http :url "http://www.cx4a.org/pub/auto-complete-emacs-lisp.el"))
 (auto-complete-extension status "installed" recipe
			  (:name auto-complete-extension :type emacswiki :description "Some extension for auto-complete-mode"))
 (browse-kill-ring status "installed" recipe
		   (:name browse-kill-ring :description "Interactively insert items from kill-ring" :type emacswiki :features browse-kill-ring))
 (buffer-move status "installed" recipe
	      (:name buffer-move :description "Swap buffers without typing C-x b on each window" :type emacswiki :features buffer-move))
 (calfw status "installed" recipe
	(:name calfw :type github :pkgname "kiwanami/emacs-calfw" :load-path "." :description "A calendar framework for Emacs (with support for `org-mode', `howm' and iCal files)" :website "https://github.com/kiwanami/emacs-calfw"))
 (cedet status "installed" recipe
	(:name cedet :website "http://cedet.sourceforge.net/" :description "CEDET is a Collection of Emacs Development Environment Tools written with the end goal of creating an advanced development environment in Emacs." :type bzr :url "bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk" :build
	       ("touch `find . -name Makefile`" "make")
	       :build/windows-nt
	       ("echo #!/bin/sh > tmp.sh & echo touch `/usr/bin/find . -name Makefile` >> tmp.sh & echo make FIND=/usr/bin/find >> tmp.sh" "sed 's/^M$//' tmp.sh  > tmp2.sh" "sh ./tmp2.sh" "rm ./tmp.sh ./tmp2.sh")
	       :load-path
	       ("./common" "speedbar")))
 (cmake-mode status "installed" recipe
	     (:name cmake-mode :website "http://www.itk.org/Wiki/CMake_Editors_Support" :description "Provides syntax highlighting and indentation for CMakeLists.txt and *.cmake source files." :type http :url "http://www.cmake.org/CMakeDocs/cmake-mode.el" :features "cmake-mode" :post-init
		    (progn
		      (add-to-list 'auto-mode-alist
				   '("CMakeLists\\.txt\\'" . cmake-mode))
		      (add-to-list 'auto-mode-alist
				   '("\\.cmake\\'" . cmake-mode)))))
 (color-theme status "installed" recipe
	      (:name color-theme :description "An Emacs-Lisp package with more than 50 color themes for your use. For questions about color-theme" :website "http://www.nongnu.org/color-theme/" :type http-tar :options
		     ("xzf")
		     :url "http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.tar.gz" :load "color-theme.el" :features "color-theme" :post-init
		     (progn
		       (color-theme-initialize)
		       (setq color-theme-is-global t))))
 (dired-single status "installed" recipe
	       (:name dired-single :description "Reuse the current dired buffer to visit another directory" :type emacswiki :features "dired-single"))
 (doxymacs status "installed" recipe
	   (:name doxymacs :website "http://doxymacs.sourceforge.net/" :description "Doxymacs is Doxygen + {X}Emacs." :type git :url "git://doxymacs.git.sourceforge.net/gitroot/doxymacs/doxymacs" :load-path
		  ("./lisp")
		  :build
		  ("./bootstrap" "./configure" "make")
		  :features doxymacs))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :features el-get :load "el-get.el"))
 (erc-highlight-nicknames status "installed" recipe
			  (:name erc-highlight-nicknames :description "Highlights nicknames" :type emacswiki :features erc-highlight-nicknames))
 (filladapt status "installed" recipe
	    (:name filladapt :description "Filladapt enhances the behavior of Emacs' fill functions by guessing the proper fill prefix in many contexts. Emacs has a built-in adaptive fill mode but Filladapt is much better." :type http :url "http://www.wonderworks.com/download/filladapt.el"))
 (flymake-point status "installed" recipe
		(:name flymake-point :description "Show flymake errors under the point in the minibuffer" :type http :url "https://bitbucket.org/brodie/dotfiles/raw/tip/.emacs.d/plugins/flymake-point.el" :features flymake-point))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/m2ym/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "m2ym/fuzzy-el" :features fuzzy))
 (git-emacs status "installed" recipe
	    (:name git-emacs :description "Yet another git emacs mode for newbies" :type github :pkgname "tsgates/git-emacs" :features git-emacs))
 (google-maps status "installed" recipe
	      (:name google-maps :description "Access Google Maps from Emacs" :type git :url "git://git.naquadah.org/google-maps.git" :features google-maps))
 (google-weather status "installed" recipe
		 (:name google-weather :description "Fetch Google Weather forecasts." :type git :url "git://git.naquadah.org/google-weather-el.git" :features
			(google-weather org-google-weather)))
 (gravatar status "installed" recipe
	   (:name gravatar :description "Get Gravatars" :type http :url "http://git.gnus.org/cgit/gnus.git/plain/lisp/gravatar.el"))
 (icomplete+ status "installed" recipe
	     (:name icomplete+ :description "Extensions to `icomplete.el'." :type emacswiki :features "icomplete+"))
 (js2-mode status "installed" recipe
	   (:name js2-mode :website "https://github.com/mooz/js2-mode#readme" :description "An improved JavaScript editing mode" :type github :pkgname "mooz/js2-mode" :prepare
		  (autoload 'js2-mode "js2-mode" nil t)))
 (json status "installed" recipe
       (:name json :description "JavaScript Object Notation parser / generator" :type http :url "http://edward.oconnor.cx/elisp/json.el" :features json))
 (magit status "installed" recipe
	(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :info "." :build
	       ("make all")
	       :build/darwin
	       `(,(concat "PATH="
			  (shell-quote-argument invocation-directory)
			  ":$PATH make all"))))
 (magithub status "installed" recipe
	   (:name magithub :description "Magit extensions for using GitHub" :type github :username "nex3" :depends magit))
 (markdown-mode status "installed" recipe
		(:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :type git :url "git://jblevins.org/git/markdown-mode.git" :post-init
		       (add-to-list 'auto-mode-alist
				    '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (mo-git-blame status "installed" recipe
	       (:name mo-git-blame :description "An interactive, iterative 'git blame' mode for Emacs" :type github :pkgname "mbunkus/mo-git-blame" :features "mo-git-blame"))
 (nognus status "installed" recipe
	 (:name nognus :description "A newsreader for GNU Emacs" :type git :url "http://git.gnus.org/gnus.git" :build
		`(("./configure" ,(concat "--with-emacs="
					  (shell-quote-argument el-get-emacs)))
		  ("make"))
		:build/windows-nt
		`(,(concat "\"make.bat " invocation-directory "\""))
		:info "texi" :load-path
		("lisp")
		:autoloads nil :features gnus-load))
 (nxhtml status "installed" recipe
	 (:type github :username "emacsmirror" :name nxhtml :type emacsmirror :description "An addon for Emacs mainly for web development." :build
		(list
		 (concat el-get-emacs " -batch -q -no-site-file -L . -l nxhtmlmaint.el -f nxhtmlmaint-start-byte-compilation"))
		:load "autostart.el"))
 (offlineimap status "installed" recipe
	      (:name offlineimap :description "Run OfflineIMAP from Emacs" :type git :url "git://git.naquadah.org/offlineimap-el.git" :features offlineimap :depends nognus))
 (org-buffers status "installed" recipe
	      (:name org-buffers :description "An Org-mode tool for buffer management" :type github :pkgname "dandavison/org-buffers"))
 (org-mode status "installed" recipe
	   (:name org-mode :website "http://orgmode.org/" :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system." :type git :url "git://orgmode.org/org-mode.git" :info "doc" :build `,(mapcar
																																			 (lambda
																																			   (target)
																																			   (list "make" target
																																				 (concat "EMACS="
																																					 (shell-quote-argument el-get-emacs))))
																																			 '("clean" "all"))
		  :load-path
		  ("." "lisp" "contrib/lisp")
		  :autoloads nil :features org-install))
 (pastebin status "installed" recipe
	   (:name pastebin :description "An Emacs interface to pastebin.com." :type emacswiki :features pastebin))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/m2ym/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "m2ym/popup-el" :features popup))
 (processing-mode status "installed" recipe
		  (:name processing-mode :description "Processing mode for Emacs. Written by Rudolf Olah. This mode is a derivative of the java-mode. It adds key-bindings for running/compiling Processing sketches and it also highlights keywords found in the Processing language, such as ``setup'', ``draw'', and ``frameRate''." :type git :url "https://git.gitorious.org/processing-emacs/processing-emacs.git" :features processing-mode))
 (pymacs status "installed" recipe
	 (:name pymacs :description "Interface between Emacs Lisp and Python" :type github :pkgname "pinard/Pymacs" :prepare
		(progn
		  (setenv "PYTHONPATH"
			  (let
			      ((pplist
				(split-string
				 (or
				  (getenv "PYTHONPATH")
				  "")
				 ":" 'omit-nulls)))
			    (mapconcat 'identity
				       (remove-duplicates
					(cons default-directory pplist)
					:test #'string= :from-end t)
				       ":")))
		  (autoload 'pymacs-load "pymacs" nil t)
		  (autoload 'pymacs-eval "pymacs" nil t)
		  (autoload 'pymacs-exec "pymacs" nil t)
		  (autoload 'pymacs-call "pymacs")
		  (autoload 'pymacs-apply "pymacs"))
		:build
		("make")))
 (python status "installed" recipe
	 (:name python :description "Python's flying circus support for Emacs" :type github :pkgname "fgallina/python.el"))
 (rainbow-delimiters status "installed" recipe
		     (:name rainbow-delimiters :website "https://github.com/jlr/rainbow-delimiters#readme" :description "Color nested parentheses, brackets, and braces according to their depth." :type github :pkgname "jlr/rainbow-delimiters" :features rainbow-delimiters))
 (rope status "installed" recipe
       (:name rope :description "A python refactoring library" :post-init
	      (progn
		(unless
		    (boundp 'pymacs-load-path)
		  (setq pymacs-load-path nil))
		(add-to-list 'pymacs-load-path default-directory))
	      :type hg :url "http://bitbucket.org/agr/rope"))
 (ropemacs status "installed" recipe
	   (:name ropemacs :description "An Emacs minor mode for using rope python refactoring library in emacs." :post-init
		  (progn
		    (unless
			(boundp 'pymacs-load-path)
		      (setq pymacs-load-path nil))
		    (add-to-list 'pymacs-load-path default-directory))
		  :depends
		  (rope ropemode)
		  :type hg :url "http://bitbucket.org/agr/ropemacs"))
 (ropemode status "installed" recipe
	   (:name ropemode :description "Common parts of ropemacs and ropevim." :post-init
		  (progn
		    (unless
			(boundp 'pymacs-load-path)
		      (setq pymacs-load-path nil))
		    (add-to-list 'pymacs-load-path default-directory))
		  :type hg :url "http://bitbucket.org/agr/ropemode"))
 (sauron status "installed" recipe
	 (:name sauron :description "enhanced tracking of the world inside and outside your emacs" :website "https://github.com/djcb/sauron" :type github :pkgname "djcb/sauron" :prepare
		(autoload 'sauron-start "sauron" "Start sauron." t)))
 (smex status "installed" recipe
       (:name smex :description "M-x interface with Ido-style fuzzy matching." :type github :pkgname "nonsequitur/smex" :features smex :post-init
	      (smex-initialize)))
 (sml-modeline status "installed" recipe
	       (:name sml-modeline :description "Show position in a scrollbar like way in mode-line" :type http :url "http://bazaar.launchpad.net/~nxhtml/nxhtml/main/download/head%3A/smlmodeline.el-20100318165023-n7kkswg6dlq8l6b3-1/sml-modeline.el" :features "sml-modeline"))
 (todochiku status "installed" recipe
	    (:name todochiku :description "A mode for interfacing with Growl, Snarl, and the like." :type emacswiki :features todochiku))
 (twittering-mode status "installed" recipe
		  (:name twittering-mode :description "Major mode for Twitter" :type github :pkgname "hayamiz/twittering-mode" :features twittering-mode :compile "twittering-mode.el"))
 (undo-tree status "installed" recipe
	    (:name undo-tree :description "Treat undo history as a tree" :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :prepare
		   (progn
		     (autoload 'undo-tree-mode "undo-tree.el" "Undo tree mode; see undo-tree.el for details" t)
		     (autoload 'global-undo-tree-mode "undo-tree.el" "Global undo tree mode" t))))
 (wikipedia-mode status "installed" recipe
		 (:name wikipedia-mode :description "Mode for editing Wikipedia articles off-line" :type emacswiki :features wikipedia-mode :post-init
			(add-to-list 'auto-mode-alist
				     '("\\.wiki\\.txt\\'" . wikipedia-mode))))
 (workgroups status "installed" recipe
	     (:name workgroups :description "Workgroups for windows (for Emacs)" :type github :pkgname "tlh/workgroups.el" :features "workgroups")))
