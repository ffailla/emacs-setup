;;;
;;; log4j mode
;;;  * http://log4j-mode.sourceforge.net/
;;;
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
;;(add-hook 'log4j-mode-hook (lambda () (linum-mode nil)))

(provide 'ffailla-log4j)