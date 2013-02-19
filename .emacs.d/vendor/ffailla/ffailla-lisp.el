(setq inferior-lisp-program "~/bin/lisp")
(setq same-window-buffer-names (delete "*inferior-lisp*" same-window-buffer-names))

;;;
;;; slime - cvs distro
;;;  * http://common-lisp.net/project/slime/
;;;  * cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
;;;  * https://github.com/technomancy/slime
;;;
(autoload 'slime "slime" "Start an inferior^_superior Lisp and connect to its Swank server." t)
(autoload 'slime-mode "slime" "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)." t)
(eval-after-load 'slime
  '(progn
     (setq slime-protocol-version 'ignore)
     ;; (slime-setup '(slime-repl slime-fuzzy))
     (slime-setup '(slime-repl))
     ))

;;;
;;; ac-slime
;;;  * http://github.com/purcell/ac-slime
;;;
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;;;
;;; paredit
;;;  * http://mumble.net/~campbell/emacs/paredit.el
;;;
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

;;;
;;; nrepl
;;;  * github.com:kingtim/nrepl.el.git
;;;
(require 'nrepl)
(autoload 'nrepl-interaction-mode "nrepl-interaction" "Minor mode for nrepl interaction from a Clojure buffer.")
(setq nrepl-history-file "~/.nrepl.history")

;;;
;;; ac-nrepl
;;;  * http://github.com:purcell/ac-nrepl.git
;;;
(require 'ac-nrepl)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

;;
;; clojure-mode
;;  * http://github.com/technomancy/clojure-mode
;;  * find . -name '*.clj' | xargs etags --regex=@/Users/ffailla/bin/clojure.tags
;;
(defun clojure-mode-setup ()
  ;;(slime-mode t)
  (nrepl-interaction-mode t)
  (ac-nrepl-setup)
  (auto-complete-mode t)
  (show-paren-mode t)
  (column-number-mode t)
  (paredit-mode t)
  (outline-minor-mode t)
  (rainbow-delimiters-mode t))

(autoload 'clojure-mode "clojure-mode" nil t)
(add-hook 'clojure-mode-hook #'clojure-mode-setup)
(add-hook 'slime-repl-mode-hook #'clojure-mode-setup)
(add-hook 'inferior-lisp-mode-hook #'clojure-mode-setup)
(add-hook 'nrepl-mode-hook #'clojure-mode-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

;;;
;;; Emacs lisp mode setup
;;;
(defun emacs-lisp-mode-setup ()
  (paredit-mode t)
  (column-number-mode t)
  (show-paren-mode t)
  (outline-minor-mode t)
  (rainbow-delimiters-mode t))

(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-mode-setup)

;;;
;;; clojurescript 
;;;
;; (defun start-clojurescript ()
;;   (interactive)
;;   ;;(setq inferior-lisp-program "/Users/ffailla/dev/clojurescript/script/repljs")
;;   (cd "/Users/ffailla/dev/clojurescript/")
;;   (run-lisp "script/repljs"))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))

;;
;; clojure.etags
;;
;; /[ \t\(]*def[a-z]* \([a-z-!]+\)/\1/
;; /[ \t\(]*ns \([a-z.]+\)/\1/

(defun create-clojure-etags (project-root)
  "Create tags file for clojure project."
  (interactive "DProject Root: ")
  (eshell-command
   (format "find %s -name \'*.clj\' | xargs %s --regex=@%s -o %s/TAGS" 
	   project-root
	   "/Applications/Emacs.app/Contents/MacOS/bin/etags" ; path-to-etags  
	   "/Users/ffailla/bin/clojure.tags"
	   project-root)))


(provide 'ffailla-lisp)
