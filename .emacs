(defvar *emacs-load-start* (current-time))

(require 'cl)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;; init env
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(toggle-scroll-bar -1)

(desktop-save-mode 1)
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
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

;;(setq visible-bell f)
;;(setq ring-bell-function 'ignore)
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))))

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
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
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/highlight-parentheses/")
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
(add-to-list 'load-path "~/.emacs.d/site-lisp/rainbow-delimiters/")
(require 'rainbow-delimiters)

;;;
;;; color-theme
;;;  * http://www.nongnu.org/color-theme/
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))
;;(color-theme-classic)
;;(color-theme-comidia)
;;(color-theme-subtle-hacker)

;;; color theme with rainbow parens
(defun color-theme-subtle-hacker-rainbow-parens ()
  "Subtle Hacker Color Theme.
Based on gnome2, but uses white for important things like comments,
and less of the unreadable tomato.  By Colin Walters <levanti@verbum.org>"
  (interactive)
  (color-theme-gnome2)
  (let ((color-theme-is-cumulative t))
    (color-theme-install
     '(color-theme-subtle-hacker
       nil
       nil
       (custom-state-face ((t (:foreground "Coral"))))
       (diary-face ((t (:bold t :foreground "IndianRed"))))
       (eshell-ls-clutter-face ((t (:bold t :foreground "DimGray"))))
       (eshell-ls-executable-face ((t (:bold t :foreground "Coral"))))
       (eshell-ls-missing-face ((t (:bold t :foreground "black"))))
       (eshell-ls-special-face ((t (:bold t :foreground "Gold"))))
       (eshell-ls-symlink-face ((t (:bold t :foreground "White"))))
       (font-lock-comment-face ((t (:foreground "White"))))
       (font-lock-constant-face ((t (:bold t :foreground "Aquamarine"))))
       (font-lock-function-name-face ((t (:bold t :foreground "MediumSlateBlue"))))
       (font-lock-string-face ((t (:italic t :foreground "LightSalmon"))))
       (font-lock-variable-name-face ((t (:italic t :bold t :foreground "Aquamarine"))))
       (gnus-cite-face-1 ((t (:foreground "dark khaki"))))
       (gnus-cite-face-2 ((t (:foreground "chocolate"))))
       (gnus-cite-face-3 ((t (:foreground "tomato"))))
       (gnus-group-mail-1-empty-face ((t (:foreground "light cyan"))))
       (gnus-group-mail-1-face ((t (:bold t :foreground "light cyan"))))
       (gnus-group-mail-2-empty-face ((t (:foreground "turquoise"))))
       (gnus-group-mail-2-face ((t (:bold t :foreground "turquoise"))))
       (gnus-group-mail-3-empty-face ((t (:foreground "tomato"))))
       (gnus-group-mail-3-face ((t (:bold t :foreground "tomato"))))
       (gnus-group-mail-low-empty-face ((t (:foreground "dodger blue"))))
       (gnus-group-mail-low-face ((t (:bold t :foreground "dodger blue"))))
       (gnus-group-news-1-empty-face ((t (:foreground "green yellow"))))
       (gnus-group-news-1-face ((t (:bold t :foreground "green yellow"))))
       (gnus-group-news-2-empty-face ((t (:foreground "dark orange"))))
       (gnus-group-news-2-face ((t (:bold t :foreground "dark orange"))))
       (gnus-group-news-3-empty-face ((t (:foreground "tomato"))))
       (gnus-group-news-3-face ((t (:bold t :foreground "tomato"))))
       (gnus-group-news-low-empty-face ((t (:foreground "yellow green"))))
       (gnus-group-news-low-face ((t (:bold t :foreground "yellow green"))))
       (gnus-header-name-face ((t (:bold t :foreground "DodgerBlue1"))))
       (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))
       (gnus-signature-face ((t (:foreground "salmon"))))
       (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
       (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))
       (gnus-summary-high-read-face ((t (:bold t :foreground "forest green"))))
       (gnus-summary-high-ticked-face ((t (:bold t :foreground "burlywood"))))
       (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground "cyan"))))
       (gnus-summary-low-ancient-face ((t (:italic t :foreground "chocolate"))))
       (gnus-summary-low-read-face ((t (:foreground "light sea green"))))
       (gnus-summary-low-ticked-face ((t (:italic t :foreground "chocolate"))))
       (gnus-summary-low-unread-face ((t (:italic t :foreground "light sea green"))))
       (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))
       (gnus-summary-normal-read-face ((t (:foreground "khaki"))))
       (gnus-summary-normal-ticked-face ((t (:foreground "sandy brown"))))
       (gnus-summary-normal-unread-face ((t (:foreground "aquamarine"))))
       (message-cited-text-face ((t (:foreground "White"))))
       (message-header-name-face ((t (:foreground "DodgerBlue1"))))
       (message-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))
       (message-header-other-face ((t (:foreground "LightSkyBlue3"))))
       (message-header-xheader-face ((t (:foreground "DodgerBlue3"))))

       (rainbow-delimiters-depth-1-face ((t (:bold t :foreground "royal blue"))))
       (rainbow-delimiters-depth-2-face ((t (:bold t :foreground "turquoise"))))
       (rainbow-delimiters-depth-3-face ((t (:bold t :foreground "tomato"))))
       (rainbow-delimiters-depth-4-face ((t (:bold t :foreground "dodger blue"))))
       (rainbow-delimiters-depth-5-face ((t (:bold t :foreground "green yellow"))))
       (rainbow-delimiters-depth-6-face ((t (:bold t :foreground "dark orange"))))
       (rainbow-delimiters-depth-7-face ((t (:bold t :foreground "slateblue1"))))
       (rainbow-delimiters-depth-8-face ((t (:bold t :foreground "salmon"))))
       (rainbow-delimiters-depth-9-face ((t (:bold t :foreground "burlywood"))))
       (rainbow-delimiters-depth-10-face ((t (:bold t :foreground "chocolate"))))
       (rainbow-delimiters-depth-11-face ((t (:bold t :foreground "light sea green"))))
       (rainbow-delimiters-depth-12-face ((t (:bold t :foreground "aquamarine"))))
       (rainbow-delimiters-depth-13-face ((t (:bold t :foreground "khaki"))))
       (rainbow-delimiters-depth-14-face ((t (:bold t :foreground "light sky blue"))))

       (rainbow-delimiters-unmatched-delimiter-face ((t (:bold t :foreground "darkgray"))))
       ))))

(color-theme-subtle-hacker-rainbow-parens)

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
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

;;;
;;; ac-slime
;;;  * http://github.com/purcell/ac-slime
;;;
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/ac-slime/")
;;(require 'ac-slime)
;;(add-hook 'slime-mode-hook 'set-up-slime-ac)

;;;
;;; slime - cvs distro
;;;  * http://common-lisp.net/project/slime/
;;;  * cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/slime/")  ; your SLIME directory
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
                                        ;(add-to-list 'load-path "/path/to/elisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/paredit/")
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

;;
;; clojure-mode
;;  * http://github.com/technomancy/clojure-mode
;;  * find . -name '*.clj' | xargs etags --regex=@/Users/ffailla/bin/clojure.tags
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/clojure-mode/")
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'clojure-test-mode "clojure-test-mode" nil t)

;;(defun swank-clojure-autoloads nil
;;  (interactive)
;;  (let ((generated-autoload-file "~/Tools/swank-clojure-enablers/swank-clojure-autoload.el"))
;;    (update-directory-autoloads "~/Tools/swank-clojure-enablers")))
;;(setq swank-clojure-classpath (directory-files "~/.clojure-jars" t ".jar$"))

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
                  "~/.emacs.d/site-lisp/cdt/clojure/clojure-1.2.0/src/jvm"
                  "~/.emacs.d/site-lisp/cdt/clojure/clojure-1.2.0/src/clj"
                  "~/.emacs.d/site-lisp/cdt/clojure/clojure-contrib-1.2.0/src/main/clojure"))))

(defun clojure-mode-setup ()
  (slime-mode t)
  (show-paren-mode t)
  ;;(highlight-parentheses-mode t)
  (paredit-mode t)
  (outline-minor-mode t)
  (column-number-mode t)
  (rainbow-delimiters-mode t)
  (progn
    (define-key clojure-mode-map "\C-cc" 'comment-region)
    (define-key clojure-mode-map "\C-cu" 'uncomment-region)
    (setq cdt-dir (expand-file-name "~/.emacs.d/site-lisp/cdt"))
    (load-file (format "%s/ide/emacs/cdt.el" cdt-dir))))

(add-hook 'clojure-mode-hook #'clojure-mode-setup)
(add-hook 'slime-repl-mode-hook #'clojure-mode-setup)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
;;(add-hook 'emacs-lisp-mode-hook #'lisp-setup)
;;(add-hook 'slime-repl-mode-hook (lambda () (highlight-parentheses-mode t)))

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
(add-to-list 'load-path "~/.emacs.d/site-lisp/nxml-mode/")
(autoload 'nxml-mode "nxml-mode" nil t)
(setq auto-mode-alist (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
                            auto-mode-alist))
(unify-8859-on-decoding-mode)

;;;
;;; xml pretty printer
;;;  * http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
;;;
(defun xml-pprint-region (begin end)
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

(defun xml-pprint ()
  (interactive)
  (push-mark)
  (xml-pprint-region (point-min) (point-max)))

;;;
;;; emacs-nav
;;;  * http://code.google.com/p/emacs-nav/
;;;  * hg clone https://emacs-nav.googlecode.com/hg/ emacs-nav
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-nav/")
(autoload 'nav "nav" nil t)

;;;
;;; magit
;;;  * http://github.com/philjackson/magit
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/")
(autoload 'magit-status "magit" nil t)

;;;
;;; mo-git-blame
;;;  * git clone git://git.bunkus.org/mo-git-blame.git
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/mo-git-blame")
;;(autoload 'mo-git-blame-file "mo-git-blame" nil t)
;;(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;;;
;;; log4j mode
;;;  * http://log4j-mode.sourceforge.net/
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/log4j-mode/")
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
;;(add-hook 'log4j-mode-hook (lambda () (linum-mode nil)))

;;;
;;; javascript mode
;;;  * http://www.emacswiki.org/emacs/JavaScriptMode
;;;  * http://www.brgeight.se/downloads/emacs/javascript.el
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/javascript/")
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

(defun javascript-mode-setup ()
  ;;(linum-mode t)
  (column-number-mode)
  )

(add-hook 'javascript-mode-hook #'javascript-mode-setup)
(autoload 'javascript-mode "javascript" nil t)

;;;
;;; org-mode
;;;  * http://orgmode.org/
;;;
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/contrib/lisp" load-path))
(require 'org-install)  ; org-install.el only has autoloads
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
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

(defun org-set-org-agenda-files ()
  (interactive)
  (setq org-agenda-files
        (directory-files "~/org" t ".org$")))

(defun org-set-org-mobile-files ()
  (interactive)
  (setq org-mobile-files (directory-files "~/org" t ".org$")))

(modify-coding-system-alist 'file "\\.org\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.dat\\'" 'utf-8)
;;(add-hook 'org-mode-hook 'org-set-mobile-org-files)
;;(add-hook 'org-mode-hook 'org-set-org-agenda-files)

;;;
;;; jdee
;;;  * http://jdee.sourceforge.net/
;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/jdee/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cedet/common"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/elib"))
;;(autoload 'cedet "cedet" nil t)
;;(autoload 'jde "jde" nil t)
;;(add-to-list 'auto-mode-alist '("\\.java\\'" . jde))
(defun jde-start ()
  (load-file (expand-file-name "~/.emacs.d/site-lisp/cedet/common/cedet.el"))
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
;;;  * DIR /S /A /ONE /B | etags -
(add-to-list 'load-path "~/.emacs.d/site-lisp/csharpmode/")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; (defun my-csharp-mode-fn ()
;;   "function that runs when csharp-mode is initialized for a buffer."
;;   ...insert your code here...
;;   ...most commonly, your custom key bindings ...
;;   )
;; (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

;;;
;;; sql-mode / jisql
;;;  * http://www.xigole.com/software/jisql/jisql.jsp
;;;
(setq exec-path (append exec-path '("~/.emacs.d/site-lisp/jisql")))
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
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/csv-mode")
;;(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;;(autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)
;;(require 'csv-mode)

;;;
;;; ecb
;;;  * http://http://ecb.sourceforge.net/
;;;  * cvs -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb login
;;;  * cvs -z3 -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb co -P modulename
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/ecb/")
;;(require 'ecb)
;;(require 'ecb-autoloads)

(defun ecb-start ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/site-lisp/cedet/common/cedet.el"))
  (semantic-load-enable-minimum-features)
  (require 'ecb))

;;;
;;; xml-rpc
;;;  * http://www.emacswiki.org/emacs/XmlRpc
;;;  * http://www.emacswiki.org/emacs/xml-rpc.el
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/xml-rpc")
(require 'xml-rpc)

;;;
;;; jira
;;;  * http://www.emacswiki.org/emacs/JiraMode
;;;  * http://www.emacswiki.org/emacs/jira.el
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/jira")

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

;;(global-set-key (kbd "C-x <up>") 'windmove-up)
;;(global-set-key (kbd "C-x <down>") 'windmove-down)
;;(global-set-key (kbd "C-x <right>") 'windmove-right)
;;(global-set-key (kbd "C-x <left>") 'windmove-left)

;;;
;;; Emacs Starter Kit fns
;;;
(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

(defalias 'yes-or-no-p 'y-or-n-p)

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
;;; start emacs server
;;;  * use /Applications/Emacs.app/Contents/MacOS/bin/emacsclient as editor for git
;;;
(if (not (and (boundp 'server-process)
              (memq (process-status server-process) '(connect listen open run))))
    (server-start))

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                                     (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))