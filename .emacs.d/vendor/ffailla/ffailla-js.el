;;;
;;; js2-mode
;;;  * http://code.google.com/p/js2-mode/
;;;
;; M-x byte-compile-file js2.el

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;
;;; js-comint
;;;  * http://js-comint-el.sourceforge.net/
;;;
(require 'js-comint)
;;(setq inferior-js-program-command "java org.mozilla.javascript.tools.shell.Main")
(setenv "NODE_NO_READLINE" "1")
(setq inferior-js-program-command "node --interactive")

(defun my-js2-mode-hook ()
  (rainbow-delimiters-mode t)
  (setq js-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)

  (define-key js2-mode-map (kbd "\C-x\C-e") 'js-send-last-sexp)
  (define-key js2-mode-map (kbd "\C-c\C-r") 'js-send-region)
  (define-key js2-mode-map (kbd "\C-\M-x") 'js-send-last-sexp-and-go)
  (define-key js2-mode-map (kbd "\C-cb") 'js-send-buffer)
  (define-key js2-mode-map (kbd "\C-c\C-b" ) 'js-send-buffer-and-go)
  (define-key js2-mode-map (kbd "\C-cl") 'js-load-file-and-go))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(provide 'ffailla-js)
