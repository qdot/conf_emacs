;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hooks for psvn
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

										; Setup psvn (for svn-status)
(require 'psvn)
										; Don't list all files when doing svn-status, only different/changed/etc
(set-variable 'svn-status-verbose nil)
										; setup vc-svn
(add-to-list 'vc-handled-backends 'SVN)
										; Set up ediff colors so that non-selected diffs are actually visible
(add-hook 'ediff-load-hook
          (lambda ()
            (set-face-foreground
             ediff-odd-diff-face-A "black")
            (set-face-foreground
             ediff-even-diff-face-A "black")
            (set-face-foreground
             ediff-odd-diff-face-B "black")
            (set-face-foreground
             ediff-even-diff-face-B "black")
            (set-face-foreground
             ediff-odd-diff-face-C "black")
            (set-face-foreground
             ediff-even-diff-face-C "black")
            ))
										; Default ediff to horizontal split instead of vertical split
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)
