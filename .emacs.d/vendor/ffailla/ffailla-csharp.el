;;;
;;; csharpmode
;;;  * svn checkout http://csharpmode.googlecode.com/svn/trunk/ csharpmode-read-only
;;;  * find . -name "*.cs" -print | etags -
;;;  * find . -name *.cs | xargs /usr/local/bin/ctags -a -e  -f TAGS
;;;  * DIR /S /A /ONE /B | etags -
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(provide 'ffailla-csharp)