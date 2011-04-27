;;;
;;; js2-mode
;;;  * http://code.google.com/p/js2-mode/
;;;
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun js2-mode-setup ()
  (rainbow-delimiters-mode t))
(add-hook 'js2-mode-hook #'js2-mode-setup)

(provide 'ffailla-js)
