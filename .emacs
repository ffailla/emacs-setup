(defvar *emacs-load-start* (current-time))

(require 'cl)

;; init env 
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin:/usr/local/bin"))
(setq exec-path (append exec-path '("/opt/local/bin" "/usr/local/bin")))

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
(setq inferior-lisp-program "/opt/local/bin/lisp")
(autoload 'slime "slime" "Start an inferior^_superior Lisp and connect to its Swank server." t)
(autoload 'slime-mode "slime" "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)." t)
(eval-after-load 'slime 
  '(progn 
     (setq slime-protocol-version 'ignore)
     (slime-setup '(slime-repl slime-fuzzy))))

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
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'clojure-test-mode "clojure-test-mode" nil t)

;;
;; setup clojure-mode hook
;;
;; cdt
;;  * http://georgejahad.com/clojure/emacs-cdt.html
;;  * git://github.com/GeorgeJahad/cdt.git
;;
(defun clojure-mode-setup ()
  (slime-mode t)
  (show-paren-mode t)
  (highlight-parentheses-mode t)
  (paredit-mode t)
  (progn
    (setq cdt-dir (expand-file-name "~/.emacs.d/site-lisp/cdt"))
    (setq cdt-source-path 
	  (reduce (lambda (acc f)
		    (concat (expand-file-name acc) ":" (expand-file-name f)))
		  '("~/.emacs.d/site-lisp/cdt/clojure/clojure-1.2.0/src/jvm"
		     "~/.emacs.d/site-lisp/cdt/clojure/clojure-1.2.0/src/clj"
		     "~/.emacs.d/site-lisp/cdt/clojure/clojure-contrib-1.2.0/src/main/clojure")))
    (load-file (format "%s/ide/emacs/cdt.el" cdt-dir))))

(add-hook 'clojure-mode-hook #'clojure-mode-setup)
(add-hook 'slime-repl-mode-hook #'clojure-mode-setup)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
;(add-hook 'emacs-lisp-mode-hook #'lisp-setup)
;(add-hook 'slime-repl-mode-hook (lambda () (highlight-parentheses-mode t)))

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
(add-to-list 'load-path "~/.emacs.d/site-lisp/ac-slime/")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;;
;; highlight-parentheses
;;  * http://nschum.de/src/emacs/highlight-parentheses/
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/highlight-parentheses/")
(autoload 'highlight-parentheses-mode "highlight-parentheses" "highlight parentheses mode" t)
(setq hl-paren-colors
      '(;;"orange1" "red1" "green1" "springgreen1" "blue1" "cyan1" "slateblue1" "magenta1" "purple"
	"grey55" "#7F9F7F" "#8CD0D3" "#DCA3A3" "#385F38" "#F0DFAF" "#BCA3A3" "#C0BED1" "#FFCFAF" "#F0EFD0" "#F0DFAF" "#DFCFAF"	
	"brown" "Darkblue" "darkgray" "darkgreen" "darkcyan" "darkred" "darkmagenta" "brown" "gray" "black" "darkmagenta" "Darkblue" "darkgreen" "darkcyan" "darkred" "red"
	"#CD4A4A" "#A5694F" "#FFA343" "#87A96B" "#17806D" "#1DACD6" "#1A4876" "#7442C8" "#FF1DCE" "#CB4154" 
))

;;
;; nxml-mode
;;  * http://www.thaiopensource.com/nxml-mode/
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/nxml-mode/")
(autoload 'nxml-mode "nxml-mode" nil t)
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
;; emacs-nav
;;  * http://code.google.com/p/emacs-nav/
;;  * hg clone https://emacs-nav.googlecode.com/hg/ emacs-nav
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-nav/")
(autoload 'nav "nav" nil t)

;;
;; magit
;;  * http://github.com/philjackson/magit
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/")
(autoload 'magit-status "magit" nil t)

;;
;; log4j mode
;;  * http://log4j-mode.sourceforge.net/
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/log4j-mode/")
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

;;
;; javascript mode
;;  * http://www.emacswiki.org/emacs/JavaScriptMode
;;  * http://www.brgeight.se/downloads/emacs/javascript.el
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/javascript/")
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;; org-mode
;;  * http://orgmode.org/
;;
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/contrib/lisp" load-path))
(require 'org-install)  ; org-install.el only has autoloads
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
;(global-font-lock-mode 1)                     ; for all buffers

;;
;; save-visited-files
;;  * http://github.com/nflath/save-visited-files
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/save-visited-files/")
(require 'save-visited-files)
(turn-on-save-visited-files-mode)

;;
;; jdee
;;  * http://jdee.sourceforge.net/
;; 
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/jdee/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cedet/common"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/elib"))
(autoload 'cedet "cedet" nil t)
(autoload 'jde "jde" nil t)
(add-to-list 'auto-mode-alist '("\\.java\\'" . jde))

;;
;; diff-mode customization
;;
(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . diff-mode))
(custom-set-faces
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
;;  * find . -name "*.cs" -print | etags -
(add-to-list 'load-path "~/.emacs.d/site-lisp/csharpmode/")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;(defun my-csharp-mode-fn ()
;  "function that runs when csharp-mode is initialized for a buffer."
;  ...insert your code here...
;  ...most commonly, your custom key bindings ...
;  )
;(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

;;
;; sql-mode / jisql
;;  * http://www.xigole.com/software/jisql/jisql.jsp
;;
(setq exec-path (append exec-path '("~/.emacs.d/site-lisp/jisql")))
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
(add-to-list (quote auto-mode-alist) (quote ("\\.actr\\'" . actr-mode)))

(setq auto-mode-alist
	(append
	 '(("\\.sp\\'"		. S-mode) ;; re: Don MacQueen <macq@llnl.gov>
	   ("\\.[qsS]\\'"	. S-mode) ;; q,s,S [see ess-restore-asm-extns above!]
	   ("\\.ssc\\'"		. S-mode) ;; Splus 4.x script files.
	   ("\\.[rR]\\'"	. R-mode)
	   ("\\.[rR]nw\\'"	. Rnw-mode)
	   ("\\.[rR]profile\\'" . R-mode)
	   ("NAMESPACE\\'"	. R-mode)
	   ("\\.omg\\'"         . omegahat-mode)
	   ("\\.hat\\'"         . omegahat-mode) ;; Duncan's pref'd...
	   ("\\.lsp\\'"		. XLS-mode)
	   ("\\.do\\'"		. STA-mode)
	   ("\\.ado\\'"		. STA-mode)
	   ("\\.[Ss][Aa][Ss]\\'"	. SAS-mode)
	   ;; Many .log/.lst files, not just SAS
	   ;;("\\.log\\'"	. SAS-log-mode)
	   ;;("\\.lst\\'"	. SAS-listing-mode)
	   ("\\.[Ss]t\\'"	. S-transcript-mode)
	   ("\\.[Ss]out"	. S-transcript-mode)
	   ("\\.[Rr]t\\'"	. R-transcript-mode)
	   ("\\.[Rr]out"	. R-transcript-mode) 
          )
	 auto-mode-alist))

;;
;; start emacs server
;;  * use /Applications/Emacs.app/Contents/MacOS/bin/emacsclient as editor for git
(server-start)

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
				     (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
