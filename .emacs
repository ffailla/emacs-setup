;; init env paths
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))
(setq exec-path (append exec-path '("/opt/local/bin")))

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
;(setq inferior-lisp-program "/opt/local/bin/lisp")
(require 'slime)
(slime-setup '(slime-repl slime-fuzzy))

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
        "orange1" "red1" "green1" "springgreen1" "blue1" "cyan1" "slateblue1" "magenta1" "purple"))

;;
;; setup clojure-mode hook
;;
(defun clojure-mode-setup ()
  (highlight-parentheses-mode t)
  (paredit-mode t)
;  (set-up-slime-ac t)
)
(add-hook 'clojure-mode-hook #'clojure-mode-setup)
;(add-hook 'slime-repl-mode-hook #'list-setup)
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
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/save-visited-files/")
(require 'save-visited-files)
(turn-on-save-visited-files-mode)

;labrepl
;(setq inferior-lisp-program "script/swank")
;(setq load-path (append (list "/Users/ffailla/dev/labrepl")))
;(setq load-path (cons "~/dev/org-mode/lisp" "/opt/local/bin" load-path))
