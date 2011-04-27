;;;
;;; jdee
;;;  * http://jdee.sourceforge.net/
;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/jdee/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/cedet/common"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/elib"))
;;(autoload 'cedet "cedet" nil t)
;;(autoload 'jde "jde" nil t)
;;(add-to-list 'auto-mode-alist '("\\.java\\'" . jde))
(defun jde-start ()
  (load-file (expand-file-name "~/.emacs.d/vendor/cedet/common/cedet.el"))
  (require 'jde))

(provide 'ffailla-java)