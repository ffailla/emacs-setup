;;;
;;; ecb
;;;  * http://http://ecb.sourceforge.net/
;;;  * cvs -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb login
;;;  * cvs -z3 -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb co -P modulename
;;;
(defun ecb-start ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/vendor/cedet/common/cedet.el"))
  (semantic-load-enable-minimum-features)
  (require 'ecb))

(provide 'ffailla-ecb)