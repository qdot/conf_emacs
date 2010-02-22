(require 'auto-install)
(setq auto-install-directory "~/.emacs_files/elisp_auto/")

(add-to-list 'auto-install-batch-list 
             '("qDot Packages" 21 9
	       (
		"http://www.skamphausen.de/cgi-bin/ska/download/active-menu.el"
		"http://www.cmake.org/CMakeDocs/cmake-mode.el"
		"http://xahlee.org/emacs/command-frequency.el"
		"http://www.emacswiki.org/emacs/download/dired-single.el"
		"http://www.emacswiki.org/emacs/download/frame-fns.el"
		"http://www.emacswiki.org/emacs/download/frame-cmds.el"
		"http://www.emacswiki.org/emacs/download/framemove.el"
		"http://www.emacswiki.org/emacs/download/icomplete+.el"
		"http://jblevins.org/projects/markdown-mode/markdown-mode.el"
		"http://github.com/rmm5t/maxframe-el/raw/master/maxframe.el"
		"http://www.emacswiki.org/emacs/download/twit.el"
		"http://www.emacswiki.org/emacs/download/xml-parse.el"
		)
))

(defun auto-install-qdot-packages ()
  "Install pacakges I normally use"
  (interactive)
  (setq auto-install-save-confirm nil)
  (setq auto-install-install-confirm nil)
  (auto-install-batch "qDot Packages")
  (setq auto-install-save-confirm t)
  (setq auto-install-install-confirm t)
)