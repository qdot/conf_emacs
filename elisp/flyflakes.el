;; Pyflakes / flymake interaction

(require 'flymake)

(provide 'flyflakes)

(defcustom flyflakes-pyflakes-command
  (if (eq system-type 'windows-nt)
      (list
       ;; XXX these paths should be discovered differently; inspect %PATH%, etc.
       "C:\\Python25\\python.exe"
       "C:\\Projects\\combinator_paths\\bincache\\pyflakes.py")
    (list
     (let ((flakesbin (shell-command-to-string "which pyflakes")))
       ;; strip the trailing newline.
       (substring flakesbin 0 -1))))
  "A list of strings, indicating the command to run to invoke
  pyflakes.  (Not necessarily just the location of the Pyflakes
  script; i.e. on Windows you need to include the Python
  interpreter as the first element of the list.)")

(defun flyflakes-temporary-file (file-name prefix)
  "Create a temporary file (actually in a temporary directory,
this need not pollute our actual source code directory)."
  (make-temp-file prefix))

(defun flyflakes-init ()
  "Create a temporary buffer, and return a list of a command-name
string and list of argument strings to pass to that command."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flyflakes-temporary-file))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (car flyflakes-pyflakes-command)
          (append (cdr flyflakes-pyflakes-command) (list local-file)))))

;; c.f. http://www.gnu.org/software/emacs/manual/html_node/flymake/Adding-support-for-a-new-syntax-check-tool.html

(defvar flyflakes-initialized nil)

(defun flyflakes-maybe-initialize ()
  "Initialize flyflakes if it hasn't been initialized yet."
  (unless flyflakes-initialized
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.py\\'" flyflakes-init))
    (setq flyflakes-initialized t)))

;;;###autoload
(defun flyflakes-mode ()
  "Add pyflakes support to flymake-mode if it hasn't been added
  yet, and then enable flymake-mode in the current buffer."
  (interactive)
  (run-at-time
   "1 sec" nil
   (lambda (the-buffer)
     ;; 'dvc-diff' has a nasty habit of opening a Python file in a buffer, then
     ;; immediately screwing around with it to get rid of buffer-file-name or
     ;; something, so that when we initialize flymake we think the
     ;; buffer-file-name is one thing but then when we get around to actually
     ;; running the pyflakes commnad it's changed and the temporary file name
     ;; doesn't exist.  This is an inelegant workaround, but it does pop the
     ;; stack and let the buffer's initial status settle down enough that the
     ;; initial value that flymake-mode sees when it runs is consistent with
     ;; its eventual value when it tries to invoke the pyflakes process.
     (save-excursion
       (set-buffer the-buffer)
       (flyflakes-maybe-initialize)
       (flymake-mode t)))
   (current-buffer)))

;;;###autoload
(defun maybe-flyflakes-mode ()
  "Maybe turn on flyflakes mode, if it's okay.
Don't initialize flyflakes on buffers that are visiting
*_flymake* buffers."
  (interactive)
  (if (eq nil (string-match ".*_flymake.*" (buffer-file-name)))
      (flyflakes-mode)))

;;;###autoload
(add-hook 'python-mode-hook 'maybe-flyflakes-mode)

(defun flyflakes-goto-next-error ()
  "An interactive command for jumping to the next flymake error,
and displaying the reason for the error at the same time."
  (interactive)
  (if (eq (length flymake-err-info) 0)
      (message "No errors in this buffer!")
    (progn
      (flymake-goto-next-error)
      (display-local-help))))

(global-set-key [?\M-,] 'flyflakes-goto-next-error)