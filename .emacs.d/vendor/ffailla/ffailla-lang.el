;;;
;;; ffailla-lang.el
;;;

;;;
;;; arduino
;;;
;; Configure arduino OS X dirs.
(setq ede-arduino-appdir "/Applications/Arduino.app/Contents/Resources/Java")

(add-to-list 'load-path "~/.emacs.d/vendor/arduino-mode")
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;;;
;;; csharpmode
;;;  * svn checkout http://csharpmode.googlecode.com/svn/trunk/ csharpmode-read-only
;;;  * find . -name "*.cs" -print | etags -
;;;  * find . -name *.cs | xargs /usr/local/bin/ctags -a -e  -f TAGS
;;;  * DIR /S /A /ONE /B | etags -
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;;
;;; cypher
;;;
(require 'cypher-mode)

;;;
;;; sh-mode helpers
;;;
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step 
      (goto-char max)
      (next-line))
    ))

(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))

(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

(define-key cypher-mode-map [(control ?c) (control ?r)] 'sh-send-line-or-region-and-step)
(define-key cypher-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)

;;;
;;; ess
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/ess/lisp")
(setq ess-r-versions nil)

(autoload 'ess-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R "ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'Rnw-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'omegahat-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'XLS-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'STA-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'SAS-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-transcript-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-transcript-mode "ess-site" "Emacs Speaks Statistics" t)

(autoload 'actr-mode "actr-mode" "ACT-R mode" 'interactive nil)
;;(add-to-list (quote auto-mode-alist) (quote ("\\.actr\\'" . actr-mode)))

(setq auto-mode-alist
      (append
       '(("\\.sp\\'"    . S-mode) ;; re: Don MacQueen <macq@llnl.gov>
         ("\\.[qsS]\\'" . S-mode) ;; q,s,S [see ess-restore-asm-extns above!]
         ("\\.ssc\\'"   . S-mode) ;; Splus 4.x script files.
         ("\\.[rR]\\'"  . R-mode)
         ("\\.[rR]nw\\'"  . Rnw-mode)
         ("\\.[rR]profile\\'" . R-mode)
         ("NAMESPACE\\'"      . R-mode)
         ("\\.omg\\'"         . omegahat-mode)
         ("\\.hat\\'"         . omegahat-mode) ;; Duncan's pref'd...
         ("\\.lsp\\'"         . XLS-mode)
         ("\\.do\\'"          . STA-mode)
         ("\\.ado\\'"         . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'"  . SAS-mode)
         ;; Many .log/.lst files, not just SAS
         ;;("\\.log\\'" . SAS-log-mode)
         ;;("\\.lst\\'" . SAS-listing-mode)
         ("\\.[Ss]t\\'" . S-transcript-mode)
         ("\\.[Ss]out"  . S-transcript-mode)
         ("\\.[Rr]t\\'" . R-transcript-mode)
         ("\\.[Rr]out"  . R-transcript-mode)
         )
       auto-mode-alist))

;;(setq inferior-R-program-name "c:/progra~1/R/R-2.2.1/bin/Rterm.exe")
;;(setq inferior-R-program-name "/Applications/R64.app/Contents/MacOS/R")


;;;
;;; haskell
;;;

;;(load "~/.emacs.d/vendor/haskell-mode/ghc-core.el")
;;(require 'haskell-mode)

;;(add-to-list 'auto-mode-alist '("\\.hs\\" . haskell-mode))

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'font-lock-mode)


;; (setq haskell-program-name
;;       (if (eq system-type 'cygwin)
;;	  "/cygdrive/c/ghc/ghc-6.8.1/bin/ghcii.sh"
;; 	"c:/ghc/ghc-6.8.1/bin/ghci.exe"))

(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hic?$"     . haskell-mode)
                ("\\.hsc$"     . haskell-mode)
		("\\.chs$"    . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)

;adding the following lines according to which modules you want to use:
;;(require 'inf-haskell)

(add-hook 'haskell-mode-hook 'turn-on-font-lock)
;(add-hook 'haskell-mode-hook 'turn-off-haskell-decl-scan)
;(add-hook 'haskell-mode-hook 'turn-off-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook 
   (function
    (lambda ()
      (setq haskell-program-name "ghci")
      (setq haskell-ghci-program-name "ghci6"))))

;;;
;;; java
;;;
;;(add-to-list 'load-path "~/.emacs.d/vendor/jdee-2.4.0.1/lisp")
;;(add-to-list 'load-path "~/.emacs.d/vendor/cedet-1.1/common")
;;(load-file (expand-file-name "~/.emacs.d/vendor/cedet-1.1/common/cedet.el"))
;;(add-to-list 'load-path (expand-file-name "~/emacs/site/elib"))

;;(require 'jde)

;;;
;;; js2-mode
;;;  * http://code.google.com/p/js2-mode/
;;;
;; M-x byte-compile-file js2.el

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js-indent-level 2)
(require 'flymake-jshint)
(add-hook 'js-mode-hook 'flymake-mode)
;;;
;;; js-comint
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

;;;
;;; objective-c / xcode
;;;  * find . \( -name "*.cpp" -o -name "*.h" -o -name "*.m" -o -name "*.mm" \) -print | etags -
;;;
(defun xcode-compile ()
  (interactive)
  (let ((df (directory-files "."))
        (has-proj-file nil))
    (while (and df (not has-proj-file))
      (let ((fn (car df)))
        (if (> (length fn) 10)
            (if (string-equal (substring fn -10) ".xcodeproj")
                (setq has-proj-file t))))
      (setq df (cdr df)))
    (if has-proj-file
        (compile "xcodebuild -configuration Debug")
      (compile "make"))))

;;;
;;; prolog
;;;  *  http://bruda.ca/emacs-prolog/
;;;
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))

;;;
;;; lisp
;;;

(setq inferior-lisp-program "~/bin/lisp")
(setq same-window-buffer-names (delete "*inferior-lisp*" same-window-buffer-names))

;;;
;;; slime - cvs distro
;;;
(autoload 'slime "slime" "Start an inferior^_superior Lisp and connect to its Swank server." t)
(autoload 'slime-mode "slime" "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)." t)
(eval-after-load 'slime
  '(progn
     (setq slime-protocol-version 'ignore)
     ;; (slime-setup '(slime-repl slime-fuzzy))
     (slime-setup '(slime-repl))
     ))

(defalias 'slc 'slime-connect)

;;;
;;; ac-slime
;;;
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;;;
;;; paredit
;;;
;;(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(require 'paredit)

;;;
;;; nrepl
;;;
(require 'nrepl)

(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl t)

;; (require 'cider)
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; (setq cider-repl-tab-command 'indent-for-tab-command)
;; (setq cider-repl-pop-to-buffer-on-connect nil)
;; (setq cider-popup-stacktraces nil)
;; (setq cider-repl-popup-stacktraces t)
;; (setq cider-auto-select-error-buffer t)
;; ;;(setq nrepl-buffer-name-separator "-")
;; ;;(setq nrepl-buffer-name-show-port t)
;; (add-to-list 'same-window-buffer-names "*cider*")
;; (add-hook 'cider-repl-mode-hook 'subword-mode)
;; (add-hook 'cider-repl-mode-hook 'paredit-mode)
;; (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;; (autoload 'cider-interaction-mode "cider-interaction" "Minor mode for cider interaction from a Clojure buffer.")

(autoload 'nrepl-interaction-mode "nrepl-interaction" "Minor mode for nrepl interaction from a Clojure buffer.")
(setq nrepl-history-file "~/.nrepl.history")

;;;
;;; ac-nrepl
;;;
(require 'ac-nrepl)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

;;;
;;; clj-refactor
;;;
(require 'clj-refactor)

;;
;; clojure-mode
;;
(defun clojure-mode-setup ()
  ;;(slime-mode t)
  ;;(cider-interaction-mode t)
  (nrepl-interaction-mode t)
  (ac-nrepl-setup)
  (auto-complete-mode t)
  (show-paren-mode t)
  (column-number-mode t)
  (paredit-mode t)
  (outline-minor-mode t)
  (rainbow-delimiters-mode t)
  (clj-refactor-mode 1))

(defun emit-form-handler (buffer form)
  (lexical-let ((form form))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
				   (nrepl-emit-result buffer (format "%s" form) t)
				   (nrepl-emit-result buffer (format "%s" value) t))
                                 (lambda (buffer out)
                                   (nrepl-emit-output buffer out t))
                                 (lambda (buffer err)
                                   (nrepl-emit-output buffer err t))
                                 (lambda (buffer)
                                   (nrepl-emit-prompt buffer)))))

;; (require 'clojure-mode)
;;(autoload 'clojure-mode "clojure-mode" nil t)
(add-hook 'clojure-mode-hook #'clojure-mode-setup)
(add-hook 'slime-repl-mode-hook #'clojure-mode-setup)
(add-hook 'inferior-lisp-mode-hook #'clojure-mode-setup)

(add-hook 'nrepl-mode-hook #'clojure-mode-setup)
(add-hook 'nrepl-repl-mode-hook
	  (lambda ()
            (nrepl-turn-on-eldoc-mode)
	    (ac-nrepl-setup)
	    (auto-complete-mode t)
	    (show-paren-mode t)
	    (paredit-mode t)
	    (outline-minor-mode t)
	    (rainbow-delimiters-mode t)))

(defun eval-and-print-form (form)
  (nrepl-send-string form
		     (emit-form-handler (nrepl-current-repl-buffer) form)
		     (nrepl-current-ns)))
 
(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
	    (ac-nrepl-setup)
	    (auto-complete-mode t)
	    (show-paren-mode t)
	    (paredit-mode t)
	    (outline-minor-mode t)
	    (rainbow-delimiters-mode t)
	    
	    (define-key nrepl-interaction-mode-map
              (kbd "C-x C-e")
	      (lambda (&optional prefix)
		(interactive "P")
		(eval-and-print-form (nrepl-last-expression))))
	    
            (define-key nrepl-interaction-mode-map
              (kbd "C-M-x")
	      (lambda (&optional prefix)
		(interactive "P")
		(let ((form (nrepl-expression-at-point)))
		  (if prefix
		      (nrepl-interactive-eval-print form)
		    (eval-and-print-form form)))))))

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
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

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

(provide 'ffailla-lang)
