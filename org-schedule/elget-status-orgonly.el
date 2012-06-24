((google-maps status "installed" recipe
	      (:name google-maps :description "Access Google Maps from Emacs" :type git :url "git://git.naquadah.org/google-maps.git" :features google-maps))
 (google-weather status "installed" recipe
		 (:name google-weather :description "Fetch Google Weather forecasts." :type git :url "git://git.naquadah.org/google-weather-el.git" :features
			(google-weather org-google-weather)))
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
		  :autoloads nil :features org-install)))
