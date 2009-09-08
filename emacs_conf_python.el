(require 'python)

(setq auto-mode-alist
      (append
       '(
         ("\\.py$"   . python-mode)
         ) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; insert 'self.' in front of member vars using c-;
;; http://nflath.com/2009/08/python-mode-customizations/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun my-insert-self ()
  "Insert self. at the beginning of the current expression."
  (interactive)
  (save-excursion
    (search-backward-regexp "[ \n\t,(-]\\|^")
    (if (not (looking-at "^"))
        (forward-char))
    (insert "self.")))
(define-key	python-mode-map	(kbd "C-;")	'my-insert-self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flymake + pyflakes syntax checking for python
;; http://plope.com/Members/chrism/flymake-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
                '("\\.py\\'" flymake-pyflakes-init)))
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

(defun my-python-mode-hook()
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (set-variable 'indent-tabs-mode nil)
  (set-variable 'py-indent-offset 4)
  (lambda () (eldoc-mode 1))
  )



