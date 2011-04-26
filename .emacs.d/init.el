(defvar *emacs-load-start* (current-time))

(require 'cl)
(require 'imenu)
(require 'recentf)
(recentf-mode 1)

;;;
;;; package manager init
(add-to-list 'load-path "~/.emacs.d/elpa/")
(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
			 ("ELPA" . "http://tromey.com/elpa/") 
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;; init env
(tool-bar-mode -1)
;;(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

(defun desktop-save-in-desktop-dir-nomessage ()
  "Save the desktop in directory `desktop-dirname'."
  (interactive)
  (if desktop-dirname
      (desktop-save desktop-dirname)
    (call-interactively 'desktop-save))
  ;;(message "Desktop saved in %s" (abbreviate-file-name desktop-dirname))
  )

(desktop-save-mode 1)
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir-nomessage)))
(savehist-mode 1)
(require 'saveplace)
(setq-default save-place t)

;;set the title bar to display the full path of the buffer
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b" ("%b - " default-directory)))))))

(set-face-attribute 'default (selected-frame) :height 100)
(set-frame-position (selected-frame) 0 0)
(set-frame-size (selected-frame) 240 70)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq default-save-buffer-coding-system 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)

;;(setq visible-bell f)
;;(setq ring-bell-function 'ignore)
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))))

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor")
(setenv "PATH" (concat "~/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/sbin:" (getenv "PATH")))
(setq exec-path (append '("~/bin" "/opt/local/bin" "/opt/local/sbin" "/usr/local/bin" "/sbin") exec-path))

(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)

;;;
;;; enable column number mode
;;;
(setq column-number-mode t)

;;;
;;; linum mode
;;;  * http://stud4.tuwien.ac.at/~e0225855/linum/linum.el
;;;
(require 'linum)
(global-linum-mode 1)
;;(setq linum-format "%d ")
;;(set-fringe-mode 20)

;;;
;;; highlight-parentheses
;;;  * http://nschum.de/src/emacs/highlight-parentheses/
;;;
;;(add-to-list 'load-path "~/.emacs.d/vendor/highlight-parentheses/")
;;(autoload 'highlight-parentheses-mode "highlight-parentheses" "highlight parentheses mode" t)
;;(setq hl-paren-colors
;;      '("orange1" "red1" "green1" "springgreen1" "blue1" "cyan1" "slateblue1" "magenta1" "purple"
;;      "grey55" "#7F9F7F" "#8CD0D3" "#DCA3A3" "#385F38" "#F0DFAF" "#BCA3A3" "#C0BED1" "#FFCFAF" "#F0EFD0" "#F0DFAF" "#DFCFAF"
;;      "brown" "Darkblue" "darkgray" "darkgreen" "darkcyan" "darkred" "darkmagenta" "brown" "gray" "black" "darkmagenta" "Darkblue" "darkgreen" "darkcyan" "darkred" "red"
;;      "#CD4A4A" "#A5694F" "#FFA343" "#87A96B" "#17806D" "#1DACD6" "#1A4876" "#7442C8" "#FF1DCE" "#CB4154"
;;))

;;;
;;; rainbow-delimeters
;;;  * http://www.emacswiki.org/emacs/RainbowDelimiters
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/rainbow-delimiters/")
(require 'rainbow-delimiters)
;;(autoload 'rainbow-delimiters "rainbow-delimiters" "rainbow delimiters mode" t)

;;;
;;; color-theme
;;;  * http://www.nongnu.org/color-theme/
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/color-theme")
(add-to-list 'load-path "~/.emacs.d/vendor/ffailla/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     ;; (require 'zenburn-rainbow)
     ;; (color-theme-zenburn-rainbow)
     (require 'subtle-hacker-rainbow)
     (color-theme-subtle-hacker-rainbow)
     ;;(color-theme-clarity-rainbow)
     ;;(color-theme-zenburn-rainbow)
     ;;(color-theme-hober)
     ;;(color-theme-classic)
     ;;(color-theme-comidia)
     ;;(color-theme-subtle-hacker)
     ))

;;;
;;; ediff
;;;
(setq ediff-split-window-function 'split-window-horizontally)

;;;
;;; Setup TRAMP mode
;;;
(setq tramp-default-method "ssh")
(setq tramp-default-user "root")
(setq tramp-default-host "localhost")
(setq tramp-chunksize 500)

;;; TRAMP beep when done downloading files
(defadvice tramp-handle-write-region
  (after tramp-write-beep-advice activate)
  " make tramp beep after writing a file."
  (interactive)
  (beep))
(defadvice tramp-handle-do-copy-or-rename-file
  (after tramp-copy-beep-advice activate)
  " make tramp beep after copying a file."
  (interactive)
  (beep))
(defadvice tramp-handle-insert-file-contents
  (after tramp-copy-beep-advice activate)
  " make tramp beep after copying a file."
  (interactive)
  (beep))

;;;
;;; printing support
;;;
(require 'printing)
(pr-update-menus)
(setq ps-printer-name "PDF_file_generator")
(setq ps-printer-name t)

(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf")))

;;;
;;; autosave/backup tmp file locations
;;;
;; Put autosave files (ie #foo#) in one place, *not* scattered all over the file system!
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)
(setq auto-save-file-name-transforms `(("\\(?:[^/]*/\\)*\\(.*\\)", (concat autosave-dir "\\1") t)))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; move custom-file mods to its own file (not in this .emacs file)
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

;;;
;;; auto-complete
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;;;
;;; ac-slime
;;;  * http://github.com/purcell/ac-slime
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/ac-slime/")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;;;
;;; slime - cvs distro
;;;  * http://common-lisp.net/project/slime/
;;;  * cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/slime/")  ; your SLIME directory
(setq inferior-lisp-program "~/bin/lisp")
(autoload 'slime "slime" "Start an inferior^_superior Lisp and connect to its Swank server." t)
(autoload 'slime-mode "slime" "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)." t)
(eval-after-load 'slime
  '(progn
     (setq slime-protocol-version 'ignore)
     (slime-setup '(slime-repl slime-fuzzy))))

;;;
;;; paredit
;;;  * http://mumble.net/~campbell/emacs/paredit.el
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/paredit/")
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

;;
;; clojure-mode
;;  * http://github.com/technomancy/clojure-mode
;;  * find . -name '*.clj' | xargs etags --regex=@/Users/ffailla/bin/clojure.tags
;;
(add-to-list 'load-path "~/.emacs.d/vendor/clojure-mode/")
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'clojure-test-mode "clojure-test-mode" nil t)

;;;
;;; cdt
;;;  * http://georgejahad.com/clojure/emacs-cdt.html
;;;  * git://github.com/GeorgeJahad/cdt.git
;;;
;; (defun cdt-set-source-path ()
;;   (interactive)
;;   (setq cdt-source-path
;;         (reduce (lambda (acc f)
;;                   (concat (expand-file-name acc) ":" (expand-file-name f)))
;;                 '("./src/main/clojure"
;;                   ;;"~/.emacs.d/vendor/cdt/clojure/clojure-1.2.0/src/jvm"
;;                   "~/.emacs.d/vendor/cdt/clojure/clojure-1.2.0/src/clj"
;;                   "~/.emacs.d/vendor/cdt/clojure/clojure-contrib-1.2.0/src/main/clojure"))))

(defun clojure-mode-setup ()
  (slime-mode t)
  (show-paren-mode t)
  ;;(highlight-parentheses-mode t)
  (paredit-mode t)
  (outline-minor-mode t)
  (column-number-mode t)
  (rainbow-delimiters-mode t)
  ;; (progn
  ;;   ;; (define-key clojure-mode-map "\C-cc" 'comment-region)
  ;;   ;; (define-key clojure-mode-map "\C-cu" 'uncomment-region)
  ;;   (setq cdt-dir (expand-file-name "~/.emacs.d/vendor/cdt"))
  ;;   (load-file (format "%s/ide/emacs/cdt.el" cdt-dir)))
  )

(add-hook 'clojure-mode-hook #'clojure-mode-setup)
(add-hook 'slime-repl-mode-hook #'clojure-mode-setup)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(defun slime-clojure ()
  (interactive)
  (setq inferior-lisp-program "~/bin/swank")
  (require 'slime)
  ;;(run-lisp)
  (start-process inferior-lisp-program "*inferior-lisp*" "lisp")
  (sit-for 5)  ;; hack for now... yuck... need to learn more elisp to do this correctly
  (slime-connect "localhost" 4005))

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

;;;
;;; nxml-mode
;;;  * http://www.thaiopensource.com/nxml-mode/
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/nxml-mode/")
(autoload 'nxml-mode "nxml-mode" nil t)
(setq auto-mode-alist (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
                            auto-mode-alist))
(unify-8859-on-decoding-mode)

;;;
;;; emacs-nav
;;;  * http://code.google.com/p/emacs-nav/
;;;  * hg clone https://emacs-nav.googlecode.com/hg/ emacs-nav
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-nav/")
(autoload 'nav "nav" nil t)

;;;
;;; magit
;;;  * http://github.com/philjackson/magit
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/magit/")
(autoload 'magit-status "magit" nil t)

;;;
;;; mo-git-blame
;;;  * git clone git://git.bunkus.org/mo-git-blame.git
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/mo-git-blame")
;;(autoload 'mo-git-blame-file "mo-git-blame" nil t)
;;(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;;;
;;; log4j mode
;;;  * http://log4j-mode.sourceforge.net/
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/log4j-mode/")
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
;;(add-hook 'log4j-mode-hook (lambda () (linum-mode nil)))

;;;
;;; javascript mode
;;;  * http://www.emacswiki.org/emacs/JavaScriptMode
;;;  * http://www.brgeight.se/downloads/emacs/javascript.el
;;;
;; (add-to-list 'load-path "~/.emacs.d/vendor/javascript/")
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;; (defun javascript-mode-setup ()
;;   ;;(linum-mode t)
;;   (column-number-mode))

;; (add-hook 'javascript-mode-hook #'javascript-mode-setup)
;; (autoload 'javascript-mode "javascript" nil t)

;;;
;;; js2-mode
;;;  * http://code.google.com/p/js2-mode/
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/js2-mode/")
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun js2-mode-setup ()
  (rainbow-delimiters-mode t))
(add-hook 'js2-mode-hook #'js2-mode-setup)

;;;
;;; org-mode
;;;  * http://orgmode.org/
;;;
(setq load-path (cons "~/.emacs.d/vendor/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/vendor/org-mode/contrib/lisp" load-path))
(require 'org-install)  ; org-install.el only has autoloads
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
;;(global-font-lock-mode 1)                     ; for all buffers

(if (not (file-exists-p "~/org")) (make-directory "~/org"))
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-default-notes-file "~/org/notes.org")
(setq org-agenda-files (directory-files "~/org" t ".org$"))
(setq org-mobile-files (directory-files "~/org" t ".org$"))
(define-key global-map "\C-cc" 'org-capture)

(modify-coding-system-alist 'file "\\.org\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.dat\\'" 'utf-8)

;;;
;;; jdee
;;;  * http://jdee.sourceforge.net/
;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/jdee/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/cedet/common"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/elib"))
;;(autoload 'cedet "cedet" nil t)
;;(autoload 'jde "jde" nil t)
;;(add-to-list 'auto-mode-alist '("\\.java\\'" . jde))
(defun jde-start ()
  (load-file (expand-file-name "~/.emacs.d/vendor/cedet/common/cedet.el"))
  (require 'jde))

;;;
;;; diff-mode customization
;;;
(add-to-list 'auto-mode-alist '(".*_EDITMSG\\'" . diff-mode))
;;(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . log-entry-mode))
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

;;;
;;; csharpmode
;;;  * svn checkout http://csharpmode.googlecode.com/svn/trunk/ csharpmode-read-only
;;;  * find . -name "*.cs" -print | etags -
;;;  * find . -name *.cs | xargs /usr/local/bin/ctags -a -e  -f TAGS
;;;  * DIR /S /A /ONE /B | etags -
(add-to-list 'load-path "~/.emacs.d/vendor/csharpmode/")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; (defun csharp-mode-setup ()
;;   (rainbow-delimiters-mode t))
;; (add-hook 'csharp-mode-hook #'csharp-mode-setup)

;;;
;;; sql-mode / jisql
;;;  * http://www.xigole.com/software/jisql/jisql.jsp
;;;
(setq exec-path (append exec-path '("~/.emacs.d/vendor/jisql")))
(defun sql-save-history-hook ()
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
(add-hook 'sql-interactive-mode-hook 'sql-save-history-hook)

;;(defun sql-add-newline-first (output)
;;  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
;;  (concat "\n" output))

;;(defun sqli-add-hooks ()
;;  "Add hooks to `sql-interactive-mode-hook'."
;;  (add-hook 'comint-preoutput-filter-functions
;;          'sql-add-newline-first))
;;  (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)

;;;
;;; ess
;;;  * http://ess.r-project.org/
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
(add-to-list (quote auto-mode-alist) (quote ("\\.actr\\'" . actr-mode)))

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

;;;
;;; csv-mode
;;;  * http://www.emacswiki.org/emacs/csv-mode.el
;;;
;;(add-to-list 'load-path "~/.emacs.d/vendor/csv-mode")
;;(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;;(autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)
;;(require 'csv-mode)

;;;
;;; ecb
;;;  * http://http://ecb.sourceforge.net/
;;;  * cvs -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb login
;;;  * cvs -z3 -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb co -P modulename
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/ecb/")
;;(require 'ecb)
;;(require 'ecb-autoloads)

(defun ecb-start ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/vendor/cedet/common/cedet.el"))
  (semantic-load-enable-minimum-features)
  (require 'ecb))

;;;
;;; xml-rpc
;;;  * http://www.emacswiki.org/emacs/XmlRpc
;;;  * http://www.emacswiki.org/emacs/xml-rpc.el
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/xml-rpc")
(require 'xml-rpc)

;;;
;;; jira
;;;  * http://www.emacswiki.org/emacs/JiraMode
;;;  * http://www.emacswiki.org/emacs/jira.el
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/jira")

(defun jira-set-url ()
  (interactive)
  ;; "http://thortech.jira.com/rpc/xmlrpc"
  (setq jira-url (read-from-minibuffer "jira-url: ")))

(defun jira-start ()
  (interactive)
  (require 'jira))

;;(add-hook 'after-init-hook 'server-start)
;;(add-hook 'server-done-hook
;;        (lambda ()
;;          (shell-command
;;           "screen -r -X select `cat ~/.emacsclient-caller`")))

;;
;; irc settings
;;
(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
(setq erc-beep-match-types '(current-nick keyword))
(setq erc-fill-column 100)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;;;
;;; custom aliases/key bindings
;;;
(defalias 'ppx 'pprint-xml)
(defalias 'ttl 'toggle-truncate-lines)
(defalias 'rnb 'rename-buffer)
(defalias 'slc 'slime-connect)
(defalias 'omps 'org-mobile-push)
(defalias 'ompl 'org-mobile-pull)

(define-key global-map "\C-cs"
  (lambda ()
    (interactive)
    (message "saving current frame and window layout")
    (setq my-favorite-frame-setup (current-frame-configuration))))

(define-key global-map "\C-cf"
  (lambda ()
    (interactive)
    (message "restoring frame and window layout")
    (set-frame-configuration my-favorite-frame-setup)))

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
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-prolog")
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))

;;;
(load-file "~/.emacs.d/vendor/ffailla/bindings.el")  ; key-bindings
(load-file "~/.emacs.d/vendor/ffailla/defuns.el")    ; elisp fns

;;;
;;; start emacs server
;;;  * use /Applications/Emacs.app/Contents/MacOS/bin/emacsclient as editor for git
;;;
(if (not (and (boundp 'server-process)
              (memq (process-status server-process) '(connect listen open run))))
    (server-start))

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                                     (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
