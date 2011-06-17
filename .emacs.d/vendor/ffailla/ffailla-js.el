;;;
;;; js2-mode
;;;  * http://code.google.com/p/js2-mode/
;;;
;; M-x set-variable RET max-specpdl-size RET 100000
;; M-x set-variable RET max-lisp-eval-depth RET 50000
;; M-x byte-compile-file js2.el

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun js2-mode-setup ()
  (rainbow-delimiters-mode t))
(add-hook 'js2-mode-hook #'js2-mode-setup)

(provide 'ffailla-js)
