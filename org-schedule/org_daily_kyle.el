(load-file "~/.emacs_files/org-schedule/emacs_org_only.el")

(defun qdot/daily-agenda-kyle ()
	(org-batch-agenda " " org-agenda-ndays 2 org-agenda-include-diary nil))

(qdot/daily-agenda-kyle)
