;;;
;;; slime - cvs distro
;;;  * http://common-lisp.net/project/slime/
;;;  * cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
;;;  * https://github.com/technomancy/slime
;;;
;;(setq inferior-lisp-program "~/bin/lisp")
(setq inferior-lisp-program "/Users/ffailla/dev/clojurescript/script/repl")
(autoload 'slime "slime" "Start an inferior^_superior Lisp and connect to its Swank server." t)
(autoload 'slime-mode "slime" "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)." t)
(eval-after-load 'slime
  '(progn
     (setq slime-protocol-version 'ignore)
     (slime-setup '(slime-repl slime-fuzzy))))

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
;;; cdt
;;;  * http://georgejahad.com/clojure/emacs-cdt.html
;;;  * git://github.com/GeorgeJahad/cdt.git
;;;
(defun cdt-set-source-path ()
  (interactive)
  (setq cdt-source-path
        (reduce (lambda (acc f)
                  (concat (expand-file-name acc) ":" (expand-file-name f)))
                '("./src/main/clojure"
                  "~/.emacs.d/vendor/cdt/clojure/clojure-1.2.0/src/jvm"
                  "~/.emacs.d/vendor/cdt/clojure/clojure-1.2.0/src/clj"
                  "~/.emacs.d/vendor/cdt/clojure/clojure-contrib-1.2.0/src/main/clojure"))))

(defun clojure-mode-setup ()
  ;;(slime-mode t)
  (show-paren-mode t)
  (paredit-mode t)
  (outline-minor-mode t)
  (column-number-mode t)
  (rainbow-delimiters-mode t)
  (progn
    (setq cdt-dir (expand-file-name "~/.emacs.d/vendor/cdt"))
    (load-file (format "%s/ide/emacs/cdt.el" cdt-dir))))

;;
;; clojure-mode
;;  * http://github.com/technomancy/clojure-mode
;;  * find . -name '*.clj' | xargs etags --regex=@/Users/ffailla/bin/clojure.tags
;;
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'clojure-test-mode "clojure-test-mode" nil t)

;; (defun clojure-mode-setup ()
;;   (slime-mode t)
;;   (show-paren-mode t)
;;   (paredit-mode t)
;;   (outline-minor-mode t)
;;   (column-number-mode t)
;;   (rainbow-delimiters-mode t))

(add-hook 'clojure-mode-hook #'clojure-mode-setup)
(add-hook 'slime-repl-mode-hook #'clojure-mode-setup)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))

;;;
;;; Emacs lisp mode setup
;;;
(defun emacs-lisp-mode-setup ()
  (paredit-mode t)
  (show-paren-mode t)
  (outline-minor-mode t)
  (column-number-mode t)
  (rainbow-delimiters-mode t))

(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-mode-setup)

(provide 'ffailla-lisp)