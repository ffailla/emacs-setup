(defvar *emacs-load-start* (current-time))

;; init env 
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin:/usr/local/bin"))
(setq exec-path (append exec-path '("/opt/local/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;
;; slime - cvs distro
;;  * http://common-lisp.net/project/slime/
;;  * cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/slime/")  ; your SLIME directory
;(setq inferior-lisp-program "/opt/sbcl/bin/sbcl") ; your Lisp system
(setq inferior-lisp-program "/opt/local/bin/lisp")
(require 'slime)
(slime-setup '(slime-repl slime-fuzzy))
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))

;;
;; paredit
;;  * http://mumble.net/~campbell/emacs/paredit.el
;;
;(add-to-list 'load-path "/path/to/elisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/paredit/")
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

;;
;; clojure-mode
;;  * http://github.com/technomancy/clojure-mode
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/clojure-mode/")
(require 'clojure-mode)
(require 'clojure-test-mode)

;;  
;; auto-complete
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

;;
;; ac-slime
;;  * http://github.com/purcell/ac-slime
;;
;(add-to-list 'load-path "~/.emacs.d/site-lisp/ac-slime/")
;(require 'ac-slime)
;(add-hook 'slime-mode-hook 'set-up-slime-ac)

;;
;; highlight-parentheses
;;  * http://nschum.de/src/emacs/highlight-parentheses/
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/highlight-parentheses/")
(require 'highlight-parentheses)
(setq hl-paren-colors
      '(;"#8f8f8f" ; this comes from Zenburn
                   ; and I guess I'll try to make the far-outer parens look likethis
        ;"orange1" "red1" "green1" "springgreen1" "blue1" "cyan1" "slateblue1" "magenta1" "purple"
	"#CD4A4A" "#A5694F" "#FFA343" "#87A96B" "#17806D" "#1DACD6" "#1A4876" "#7442C8" "#FF1DCE" "#CB4154" 
	"#CD4A4A" "#A5694F" "#FFA343" "#87A96B" "#17806D" "#1DACD6" "#1A4876" "#7442C8" "#FF1DCE" "#CB4154"
	"#CD4A4A" "#A5694F" "#FFA343" "#87A96B" "#17806D" "#1DACD6" "#1A4876" "#7442C8" "#FF1DCE" "#CB4154"
	"#CD4A4A" "#A5694F" "#FFA343" "#87A96B" "#17806D" "#1DACD6" "#1A4876" "#7442C8" "#FF1DCE" "#CB4154"
))

;;
;; setup clojure-mode hook
;;
(defun clojure-mode-setup ()
  (highlight-parentheses-mode t)
  (paredit-mode t))

(add-hook 'clojure-mode-hook #'clojure-mode-setup)
;(add-hook 'slime-repl-mode-hook (lambda () (highlight-parentheses-mode t)))
(add-hook 'slime-repl-mode-hook #'clojure-mode-setup)
;(add-hook 'emacs-lisp-mode-hook #'lisp-setup)

;;
;; nxml-mode
;;  * http://www.thaiopensource.com/nxml-mode/
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/nxml-mode/")
(load "~/.emacs.d/site-lisp/nxml-mode/rng-auto.el")
(setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	      auto-mode-alist))
(unify-8859-on-decoding-mode)

;;
;; xml pretty printer
;;  * http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
;;
(defun pprint-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(defun pprint-xml ()
  (interactive)
  (push-mark)
  (pprint-xml-region (point-min) (point-max)))

;;
;; slime java helers
;;
(defun slime-java-describe (symbol-name)
  "Get details on Java class/instance at point."
  (interactive (list (slime-read-symbol-name "Java Class/instance: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (save-excursion
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(require 'clojure.contrib.repl-utils)" "\n" "(clojure.contrib.repl-utils/show " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1))))

(defun slime-javadoc (symbol-name)
  "Get JavaDoc documentation on Java class at point."
  (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max))
  (insert (concat "(require 'clojure.contrib.repl-utils)" "\n" "(clojure.contrib.repl-utils/javadoc " symbol-name ")"))
  (when symbol-name
    (slime-repl-return)
    (other-window 1)))

;;
;; emacs-nav
;;  * http://code.google.com/p/emacs-nav/
;;  * hg clone https://emacs-nav.googlecode.com/hg/ emacs-nav
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-nav/")
(require 'nav)

;;
;; magit
;;  * http://github.com/philjackson/magit
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/")
(require 'magit)

;;
;; log4j mode
;;  * http://log4j-mode.sourceforge.net/
(add-to-list 'load-path "~/.emacs.d/site-lisp/log4j-mode/")
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

;;
;; javascript mode
;;  * http://www.emacswiki.org/emacs/JavaScriptMode
;;  * http://www.brgeight.se/downloads/emacs/javascript.el
(add-to-list 'load-path "~/.emacs.d/site-lisp/javascript/")
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;; org-mode
;;  * http://orgmode.org/
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/contrib/lisp" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;(global-font-lock-mode 1)                     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only

;;
;; save-visited-files
;;  * http://github.com/nflath/save-visited-files
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/save-visited-files/")
(require 'save-visited-files)
(turn-on-save-visited-files-mode)

;;labrepl
;;(setq inferior-lisp-program "script/swank")
;;(setq load-path (append (list "/Users/ffailla/dev/labrepl")))
;;(setq load-path (cons "~/dev/org-mode/lisp" "/opt/local/bin" load-path))

;;
;; jdee
;;  * http://jdee.sourceforge.net/
;; 
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/jdee/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cedet/common"))
(load-file (expand-file-name "~/.emacs.d/site-lisp/cedet/common/cedet.el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/elib"))
(require 'jde)

;;
;; diff-mode customization
;;
(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . diff-mode))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "#559944"))))
 '(diff-context ((t nil)))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-function ((t (:foreground "#00bbdd"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-hunk-header ((t (:foreground "#fbde2d"))))
 '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
 '(diff-removed ((t (:foreground "#de1923")))))(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "#559944"))))
 '(diff-context ((t nil)))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-function ((t (:foreground "#00bbdd"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-hunk-header ((t (:foreground "#fbde2d"))))
 '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
 '(diff-removed ((t (:foreground "#de1923")))))

;;
;; csharpmode
;;  * svn checkout http://csharpmode.googlecode.com/svn/trunk/ csharpmode-read-only
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/csharpmode/")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;find . -name "*.cs" -print | etags -

;(defun my-csharp-mode-fn ()
;  "function that runs when csharp-mode is initialized for a buffer."
;  ...insert your code here...
;  ...most commonly, your custom key bindings ...
;  )
;(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)


;;
;; sql-mode
;;
(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
	(rval 'sql-product))
    (if (symbol-value rval)
	(let ((filename 
	       (concat "~/.emacs.d/sql/"
		       (symbol-name (symbol-value rval))
		       "-history.sql")))
	  (set (make-local-variable lval) filename))
      (error
         (format "SQL history will not be saved because %s is nil"
                 (symbol-name rval))))))
(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)
(setq exec-path (append exec-path '("~/.emacs.d/site-lisp/jisql")))

;;(defun sql-add-newline-first (output)
;;  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
;;  (concat "\n" output))

;;(defun sqli-add-hooks ()
;;  "Add hooks to `sql-interactive-mode-hook'."
;;  (add-hook 'comint-preoutput-filter-functions
;;	    'sql-add-newline-first))
;;  (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)

;;
;; ess
;;  * http://ess.r-project.org/
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/ess/lisp")
(require 'ess-site)
;(setq inferior-R-program-name "/Applications/R64.app/Contents/MacOS/R")

;;
;; start emacs server
;;  * use /Applications/Emacs.app/Contents/MacOS/bin/emacsclient as editor for git
(server-start)

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
				     (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))