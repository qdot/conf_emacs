((ace-jump-mode status "installed" recipe
								(:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (adoc-mode status "installed" recipe
						(:name adoc-mode :website "https://github.com/sensorflo/adoc-mode/wiki" :description "A major-mode for editing AsciiDoc files in Emacs." :type github :pkgname "sensorflo/adoc-mode" :depends markup-faces))
 (auto-complete status "installed" recipe
								(:name auto-complete :website "http://cx4a.org/software/auto-complete/" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
											 (popup fuzzy)))
 (auto-complete-clang status "installed" recipe
											(:name auto-complete-clang :website "https://github.com/brianjcj/auto-complete-clang" :description "Auto-complete sources for Clang. Combine the power of AC, Clang and Yasnippet." :type github :pkgname "brianjcj/auto-complete-clang" :depends auto-complete))
 (auto-complete-css status "installed" recipe
										(:name auto-complete-css :description "Auto-complete sources for CSS" :type http :url "http://www.cx4a.org/pub/auto-complete-css.el" :depends auto-complete))
 (auto-complete-emacs-lisp status "installed" recipe
													 (:name auto-complete-emacs-lisp :description "Auto-complete sources for emacs lisp" :type http :url "http://www.cx4a.org/pub/auto-complete-emacs-lisp.el" :depends auto-complete))
 (auto-complete-extension status "installed" recipe
													(:name auto-complete-extension :type emacswiki :description "Some extension for auto-complete-mode" :depends auto-complete))
 (bbdb status "installed" recipe
			 (:name bbdb :website "http://bbdb.sourceforge.net/" :description "The Insidious Big Brother Database (BBDB) is a contact management utility." :type git :url "git://git.savannah.nongnu.org/bbdb.git" :load-path
							("./lisp")
							:build
							`("autoconf" ,(concat "./configure --with-emacs=" el-get-emacs)
								"make clean" "rm -f lisp/bbdb-autoloads.el" "make bbdb")
							:features bbdb-loaddefs :autoloads nil :post-init
							(bbdb-initialize)))
 (browse-kill-ring status "installed" recipe
									 (:name browse-kill-ring :description "Interactively insert items from kill-ring" :type github :pkgname "browse-kill-ring/browse-kill-ring"))
 (buffer-move status "installed" recipe
							(:name buffer-move :description "Swap buffers without typing C-x b on each window" :type emacswiki :features buffer-move))
 (calfw status "installed" recipe
				(:name calfw :type github :pkgname "kiwanami/emacs-calfw" :load-path "." :description "A calendar framework for Emacs (with support for `org-mode', `howm' and iCal files)" :website "https://github.com/kiwanami/emacs-calfw"))
 (cedet status "installed" recipe
				(:name cedet :website "http://cedet.sourceforge.net/" :description "CEDET is a Collection of Emacs Development Environment Tools written with the end goal of creating an advanced development environment in Emacs." :type bzr :url "bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk" :build
							 `(("sh" "-c" "touch `find . -name Makefile`")
								 ("make" ,(format "EMACS=%s"
																	(shell-quote-argument el-get-emacs))
									"clean-all")
								 ("make" ,(format "EMACS=%s"
																	(shell-quote-argument el-get-emacs))))
							 :build/berkeley-unix
							 `(("sh" "-c" "touch `find . -name Makefile`")
								 ("gmake" ,(format "EMACS=%s"
																	 (shell-quote-argument el-get-emacs))
									"clean-all")
								 ("gmake" ,(format "EMACS=%s"
																	 (shell-quote-argument el-get-emacs))))
							 :build/windows-nt
							 ("echo #!/bin/sh > tmp.sh & echo touch `/usr/bin/find . -name Makefile` >> tmp.sh & echo make FIND=/usr/bin/find >> tmp.sh" "sed 's/^M$//' tmp.sh  > tmp2.sh" "sh ./tmp2.sh" "rm ./tmp.sh ./tmp2.sh")
							 :features nil :lazy nil :post-init
							 (unless
									 (featurep 'cedet-devel-load)
								 (load
									(expand-file-name "cedet-devel-load.el" pdir)))))
 (cmake-mode status "installed" recipe
						 (:name cmake-mode :website "http://www.itk.org/Wiki/CMake_Editors_Support" :description "Provides syntax highlighting and indentation for CMakeLists.txt and *.cmake source files." :type http :url "http://www.cmake.org/CMakeDocs/cmake-mode.el" :before
										(progn
											(autoload 'cmake-mode "cmake-mode" "Major mode for editing CMake listfiles.")
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
 (diminish status "installed" recipe
					 (:name diminish :description "An Emacs package that diminishes the amount of space taken on the mode line by the names of minor modes." :type http :url "http://www.eskimo.com/~seldon/diminish.el" :features diminish))
 (dired-single status "installed" recipe
							 (:name dired-single :description "Reuse the current dired buffer to visit another directory" :type emacswiki :features "dired-single"))
 (doxymacs status "installed" recipe
					 (:name doxymacs :website "http://doxymacs.sourceforge.net/" :description "Doxymacs is Doxygen + {X}Emacs." :type git :url "git://doxymacs.git.sourceforge.net/gitroot/doxymacs/doxymacs" :load-path
									("./lisp")
									:build
									("./bootstrap" "./configure" "make")
									:features doxymacs))
 (dynamic-fonts status "installed" recipe
								(:name dynamic-fonts :depends persistent-soft :type github :description "Set faces based on available fonts" :pkgname "rolandwalker/dynamic-fonts"))
 (el-get status "installed" recipe
				 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (elisp-slime-nav status "installed" recipe
									(:name elisp-slime-nav :type github :pkgname "purcell/elisp-slime-nav" :description "Slime-style navigation for Emacs Lisp" :prepare
												 (add-hook 'emacs-lisp-mode-hook
																	 (defun turn-elisp-slime-nav-on nil
																		 (elisp-slime-nav-mode t)))))
 (erc-highlight-nicknames status "installed" recipe
													(:name erc-highlight-nicknames :description "Highlights nicknames" :type emacswiki :features erc-highlight-nicknames))
 (eshell-manual status "installed" recipe
								(:name eshell-manual :description "eshell is great but lacks a good manual, someone wrote one." :type github :pkgname "aidalgol/eshell-manual" :build
											 (("make" "eshell.info"))
											 :compile nil :info "eshell.info"))
 (expand-region status "installed" recipe
								(:name expand-region :type github :pkgname "magnars/expand-region.el" :description "Expand region increases the selected region by semantic units. Just keep pressing the key until it selects what you want." :website "https://github.com/magnars/expand-region.el#readme" :features expand-region))
 (fill-column-indicator status "installed" recipe
												(:name fill-column-indicator :type github :website "https://github.com/alpaker/Fill-Column-Indicator#readme" :description "An Emacs minor mode that graphically indicates the fill column." :pkgname "alpaker/Fill-Column-Indicator" :features fill-column-indicator))
 (filladapt status "installed" recipe
						(:name filladapt :description "Filladapt enhances the behavior of Emacs' fill functions by guessing the proper fill prefix in many contexts. Emacs has a built-in adaptive fill mode but Filladapt is much better." :type http :url "http://www.wonderworks.com/download/filladapt.el"))
 (flymake-point status "installed" recipe
								(:name flymake-point :description "Show flymake errors under the point in the minibuffer" :type http :url "https://bitbucket.org/brodie/dotfiles/raw/tip/.emacs.d/plugins/flymake-point.el" :features flymake-point))
 (fuzzy status "installed" recipe
				(:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (git-emacs status "installed" recipe
						(:name git-emacs :description "Yet another git emacs mode for newbies" :type github :pkgname "tsgates/git-emacs" :features git-emacs))
 (gnus status "installed" recipe
			 (:name gnus :description "A newsreader for GNU Emacs" :type git :url "http://git.gnus.org/gnus.git" :build
							`(("./configure" ,(concat "--with-emacs="
																				(shell-quote-argument el-get-emacs)))
								("make"))
							:build/windows-nt
							`(,(concat "\"make.bat " invocation-directory "\""))
							:info "texi" :load-path
							("lisp")
							:autoloads nil :features gnus-load))
 (google-maps status "installed" recipe
							(:name google-maps :description "Access Google Maps from Emacs" :type git :url "git://git.naquadah.org/google-maps.git" :features google-maps))
 (google-weather status "installed" recipe
								 (:name google-weather :description "Fetch Google Weather forecasts." :type git :url "git://git.naquadah.org/google-weather-el.git" :features
												(google-weather org-google-weather)))
 (gravatar status "installed" recipe
					 (:name gravatar :description "Get Gravatars" :type http :url "http://git.gnus.org/cgit/gnus.git/plain/lisp/gravatar.el"))
 (haskell-mode status "installed" recipe
							 (:name haskell-mode :description "A Haskell editing mode" :type github :pkgname "haskell/haskell-mode" :load "haskell-site-file.el" :post-init
											(progn
												(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
												(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))))
 (icomplete+ status "installed" recipe
						 (:name icomplete+ :description "Extensions to `icomplete.el'." :type emacswiki :features "icomplete+"))
 (ido-ubiquitous status "installed" recipe
								 (:name ido-ubiquitous :description "Use ido (nearly) everywhere" :type elpa))
 (js2-mode status "installed" recipe
					 (:name js2-mode :website "https://github.com/mooz/js2-mode#readme" :description "An improved JavaScript editing mode" :type github :pkgname "mooz/js2-mode" :prepare
									(autoload 'js2-mode "js2-mode" nil t)))
 (json status "installed" recipe
			 (:name json :description "JavaScript Object Notation parser / generator" :type http :url "http://edward.oconnor.cx/elisp/json.el" :features json))
 (magit status "installed" recipe
				(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :info "." :autoloads
							 ("50magit")
							 :build
							 (("make" "all"))
							 :build/darwin
							 `(("make" ,(format "EMACS=%s" el-get-emacs)
									"all"))))
 (magit-view-file status "installed" recipe
									(:name magit-view-file :type github :description "View git file through history." :pkgname "renard/magit-view-file"))
 (magithub status "installed" recipe
					 (:name magithub :description "Magit extensions for using GitHub" :type github :pkgname "nex3/magithub" :depends magit))
 (markdown-mode status "installed" recipe
								(:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :type git :url "git://jblevins.org/git/markdown-mode.git" :before
											 (add-to-list 'auto-mode-alist
																		'("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (markup-faces status "installed" recipe
							 (:name markup-faces :description "Collection of faces for markup language modes." :type github :pkgname "sensorflo/markup-faces"))
 (mo-git-blame status "installed" recipe
							 (:name mo-git-blame :description "An interactive, iterative 'git blame' mode for Emacs" :type github :pkgname "mbunkus/mo-git-blame" :features "mo-git-blame"))
 (mu4e status "installed" recipe
			 (:name mu4e :website "http://www.djcbsoftware.nl/code/mu/mu4e.html" :description "An emacs-based e-mail client which uses mu (http://www.djcbsoftware.nl/code/mu/) as its back-end: mu4e." :type github :pkgname "djcb/mu" :post-init
							(setq mu4e-mu-binary
										(expand-file-name "mu"
																			(expand-file-name "mu"
																												(el-get-package-directory 'mu4e))))
							:build
							`(("autoreconf -i")
								("./configure")
								("make"))
							:load-path "mu4e"))
 (multiple-cursors status "installed" recipe
									 (:name multiple-cursors :description "An experiment in adding multiple cursors to emacs" :type github :pkgname "magnars/multiple-cursors.el" :features multiple-cursors))
 (nognus status "installed" recipe
				 (:name nognus :description "A newsreader for GNU Emacs" :type builtin :depends gnus))
 (nxhtml status "installed" recipe
				 (:type github :pkgname "emacsmirror/nxhtml" :name nxhtml :type emacsmirror :description "An addon for Emacs mainly for web development." :build
								(list
								 (concat el-get-emacs " -batch -q -no-site-file -L . -l nxhtmlmaint.el -f nxhtmlmaint-start-byte-compilation"))
								:load "autostart.el"))
 (offlineimap status "installed" recipe
							(:name offlineimap :description "Run OfflineIMAP from Emacs" :type git :url "git://git.naquadah.org/offlineimap-el.git" :features offlineimap))
 (org-buffers status "installed" recipe
							(:name org-buffers :description "An Org-mode tool for buffer management" :type github :pkgname "dandavison/org-buffers"))
 (org-mode status "installed" recipe
					 (:name org-mode :website "http://orgmode.org/" :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system." :type git :url "git://orgmode.org/org-mode.git" :info "doc" :build/berkeley-unix `,(mapcar
																																																																																																																																																			 (lambda
																																																																																																																																																				 (target)
																																																																																																																																																				 (list "gmake" target
																																																																																																																																																							 (concat "EMACS="
																																																																																																																																																											 (shell-quote-argument el-get-emacs))))
																																																																																																																																																			 '("oldorg"))
									:build `,(mapcar
														(lambda
															(target)
															(list "make" target
																		(concat "EMACS="
																						(shell-quote-argument el-get-emacs))))
														'("oldorg"))
									:load-path
									("." "lisp" "contrib/lisp")))
 (org-sync status "installed" recipe
					 (:name org-sync :website "http://orgmode.org/worg/org-contrib/gsoc2012/student-projects/org-sync/" :description "Org-sync is a tool to synchronize Org-mode documents with bugtracking tools such as Bugzilla, Github or Google Code and other TODO-list services such as Remember the Milk." :type git :url "git://orgmode.org/org-sync.git" :prepare
									(progn
										(autoload 'os "os" "Sync org-mode files with different online services" t)
										(autoload 'os-import "os" "Import information to sync with org-mode from different online services" t)
										(autoload 'os-sync "os" "Sync current org-mode file with service it is bound to" t))))
 (package status "installed" recipe
					(:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin 24 :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
								 (progn
									 (setq package-user-dir
												 (expand-file-name
													(convert-standard-filename
													 (concat
														(file-name-as-directory default-directory)
														"elpa")))
												 package-directory-list
												 (list
													(file-name-as-directory package-user-dir)
													"/usr/share/emacs/site-lisp/elpa/"))
									 (make-directory package-user-dir t)
									 (unless
											 (boundp 'package-subdirectory-regexp)
										 (defconst package-subdirectory-regexp "^\\([^.].*\\)-\\([0-9]+\\(?:[.][0-9]+\\)*\\)$" "Regular expression matching the name of\n a package subdirectory. The first subexpression is the package\n name. The second subexpression is the version string."))
									 (setq package-archives
												 '(("ELPA" . "http://tromey.com/elpa/")
													 ("gnu" . "http://elpa.gnu.org/packages/")
													 ("marmalade" . "http://marmalade-repo.org/packages/")
													 ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (parenface status "installed" recipe
						(:name parenface :description "Provide a face for parens in lisp modes." :type http :url "http://www.davep.org/emacs/parenface.el" :features "parenface"))
 (pastebin status "installed" recipe
					 (:name pastebin :description "An Emacs interface to pastebin.com." :type emacswiki :features pastebin))
 (persistent-soft status "installed" recipe
									(:name persistent-soft :type github :description "Persistent storage, returning nil on failure" :pkgname "rolandwalker/persistent-soft"))
 (popup status "installed" recipe
				(:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el"))
 (popwin status "installed" recipe
				 (:name popwin :description "Popup Window Manager." :website "https://github.com/m2ym/popwin-el" :type github :pkgname "m2ym/popwin-el"))
 (pymacs status "installed" recipe
				 (:name pymacs :description "Interface between Emacs Lisp and Python" :type github :pkgname "pinard/Pymacs" :prepare
								(progn
									(el-get-envpath-prepend "PYTHONPATH" default-directory)
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
										 (:name rainbow-delimiters :website "https://github.com/jlr/rainbow-delimiters#readme" :description "Color nested parentheses, brackets, and braces according to their depth." :type github :pkgname "jlr/rainbow-delimiters"))
 (rainbow-mode status "installed" recipe
							 (:name rainbow-mode :description "Colorize color names in buffers" :type elpa))
 (revbufs status "installed" recipe
					(:name revbufs :description "Reverts out-of-date buffers safely" :type http :url "http://www.neilvandyke.org/revbufs/revbufs.el" :features revbufs))
 (rope status "installed" recipe
			 (:name rope :description "A python refactoring library" :post-init
							(el-get-envpath-prepend "PYTHONPATH" default-directory)
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
 (slime status "installed" recipe
				(:name slime :description "Superior Lisp Interaction Mode for Emacs" :type github :autoloads "slime-autoloads" :info "doc" :pkgname "nablaone/slime" :load-path
							 ("." "contrib")
							 :compile
							 (".")
							 :build
							 '(("make" "-C" "doc" "slime.info"))
							 :post-init
							 (slime-setup)))
 (smex status "installed" recipe
			 (:name smex :description "M-x interface with Ido-style fuzzy matching." :type github :pkgname "nonsequitur/smex" :features smex :post-init
							(smex-initialize)))
 (sml-modeline status "installed" recipe
							 (:name sml-modeline :description "Show position in a scrollbar like way in mode-line" :type http :url "http://bazaar.launchpad.net/~nxhtml/nxhtml/main/download/head%3A/smlmodeline.el-20100318165023-n7kkswg6dlq8l6b3-1/sml-modeline.el" :features "sml-modeline"))
 (sunrise-commander status "installed" recipe
										(:name sunrise-commander :description "Two-pane file manager for Emacs based on Dired and inspired by MC" :type elpa))
 (tomorrow-night-paradise-theme status "installed" recipe
																(:name tomorrow-night-paradise-theme :description "A light-on-dark Emacs theme which is essentially a tweaked version of Chris Kempson's Tomorrow Night Bright theme." :website "https://github.com/jimeh/tomorrow-night-paradise-theme.el" :type github :pkgname "jimeh/tomorrow-night-paradise-theme.el" :minimum-emacs-version 24 :post-init
																			 (add-to-list 'custom-theme-load-path default-directory)))
 (tomorrow-theme status "installed" recipe
								 (:name tomorrow-theme :description "Colour Schemes for Hackers" :website "https://github.com/chriskempson/tomorrow-theme" :type github :pkgname "chriskempson/tomorrow-theme" :load-path "GNU Emacs" :minimum-emacs-version 24 :post-init
												(add-to-list 'custom-theme-load-path default-directory)))
 (twittering-mode status "installed" recipe
									(:name twittering-mode :description "Major mode for Twitter" :type github :pkgname "hayamiz/twittering-mode" :features twittering-mode :compile "twittering-mode.el"))
 (ucs-utils status "installed" recipe
						(:name ucs-utils :type github :description "Utilities for Unicode characters in Emacs" :pkgname "rolandwalker/ucs-utils"))
 (undo-tree status "installed" recipe
						(:name undo-tree :description "Treat undo history as a tree" :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :prepare
									 (progn
										 (autoload 'undo-tree-mode "undo-tree.el" "Undo tree mode; see undo-tree.el for details" t)
										 (autoload 'global-undo-tree-mode "undo-tree.el" "Global undo tree mode" t))))
 (unicode-fonts status "installed" recipe
								(:name unicode-fonts :depends
											 (dynamic-fonts ucs-utils)
											 :type github :description "Configure Unicode fonts" :pkgname "rolandwalker/unicode-fonts"))
 (wikipedia-mode status "installed" recipe
								 (:name wikipedia-mode :description "Mode for editing Wikipedia articles off-line" :type emacswiki :features wikipedia-mode :post-init
												(add-to-list 'auto-mode-alist
																		 '("\\.wiki\\.txt\\'" . wikipedia-mode))))
 (workgroups status "installed" recipe
						 (:name workgroups :description "Workgroups for windows (for Emacs)" :type github :pkgname "tlh/workgroups.el" :features "workgroups"))
 (yasnippet status "installed" recipe
						(:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :features "yasnippet" :pre-init
									 (unless
											 (or
												(boundp 'yas/snippet-dirs)
												(get 'yas/snippet-dirs 'customized-value))
										 (setq yas/snippet-dirs
													 (list
														(concat el-get-dir
																		(file-name-as-directory "yasnippet")
																		"snippets"))))
									 :post-init
									 (put 'yas/snippet-dirs 'standard-value
												(list
												 (list 'quote
															 (list
																(concat el-get-dir
																				(file-name-as-directory "yasnippet")
																				"snippets")))))
									 :compile nil :submodule nil)))
