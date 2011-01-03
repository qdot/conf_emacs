;; file extension mode recognition
(setq auto-mode-alist
      (append
       '(
         ("\\.py$"   . python-mode)
	 ("\\.\\(xml\\|mxml\\|html\\|htm\\)$" . nxml-mode)
	 ("\\.css$" . css-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.testdoc$" . adoc-mode)
         ("\\.archdoc$" . adoc-mode)
         ("\\.asciidoc$" . adoc-mode)
         ("\\.php$"  . php-mode)
         ("\\.py$"   . python-mode)
         ("\\.sql$"  . sql-mode)
         ("\\.mode$" . cmake-mode)
         ("\\.php$" . php-mode)
         ("\\.wiki$" . wikipedia-mode)
         ("\\.json$" . python-mode)
         ("\\.cmake$" . cmake-mode)
         ("\\.hs$" . haskell-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("ChangeLog\\.txt\\'" . change-log-mode)
         ("\\.lua$" . lua-mode)
         ) auto-mode-alist))
