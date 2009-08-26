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

(defun my-python-mode-hook()
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (set-variable 'indent-tabs-mode nil)
  (set-variable 'py-indent-offset 4)
  (lambda () (eldoc-mode 1))
)
(add-hook 'python-mode-hook 'my-python-mode-hook)

