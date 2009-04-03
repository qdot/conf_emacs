(setq auto-mode-alist
      (append
       '(
		 ("\\.py$"   . python-mode)
		 ) auto-mode-alist))

(defun my-python-mode-hook()
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (set-variable 'indent-tabs-mode nil)
  (set-variable 'py-indent-offset 4)
)
(add-hook 'python-mode-hook 'my-python-mode-hook)
