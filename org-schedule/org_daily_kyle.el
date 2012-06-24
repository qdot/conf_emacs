(load-file "~/.emacs_files_24/org-schedule/emacs_org_only.el")

(defun qdot/daily-agenda-kyle ()
	(org-batch-agenda " " org-agenda-ndays 2 org-agenda-include-diary nil))

(qdot/daily-agenda-kyle)
