;;
;; ffailla-emacs.el
;;

(require 'cl)
(require 'imenu)
(require 'recentf)

;;set the title bar to display the full path of the buffer
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b" ("%b - " default-directory)))))))

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq default-save-buffer-coding-system 'utf-8)

(setq font-lock-verbose nil) 

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-splash-screen t)
(recentf-mode 1)
(setq column-number-mode t)

(put 'downcase-region 'disabled nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

;;—————————————————————–
;; Mac Specific Configuration
;;—————————————————————–
;; Copy and Paste
;; (defun copy-from-osx ()
;;   (shell-command-to-string “pbpaste”))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process “pbcopy” “*Messages*” “pbcopy”)))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; ;; Override defaults to use the mac copy and paste
;; (setq interprogram-cut-function ‘paste-to-osx)
;; (setq interprogram-paste-function ‘copy-from-osx)

;; ;; System-specific configuration
;; (if (not (eq system-type 'windows-nt))
;;   (let ((system-type-config (concat emacs-dir (symbol-name system-type) “.el”)))
;;     (if (file-exists-p system-type-config)
;; 	(load system-type-config))))

;;;
;;; linum settings
;;;
;;(require 'linum)
(require 'hlinum)
(when (fboundp 'fringe-mode) (fringe-mode 0))
(global-linum-mode 1)
(setq linum-format "%d ")
(require 'linum-off)
;;(require 'linum+)
(setq linum-disabled-modes-list '(eshell-mode 
				  wl-summary-mode 
				  compilation-mode 
				  erc-mode 
				  org-mode
				  grep-mode))

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

;;(setq visible-bell f)
;;(setq ring-bell-function 'ignore)
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit
				      mwheel-scroll down up next-line previous-line
				      backward-char forward-char))
          (ding))))

(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)

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
;;; ediff
;;;
(setq ediff-split-window-function 'split-window-horizontally)

;;;
;;; auto-complete
;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;;;
;;; shell
;;;
;; (setq shell-file-name "C:/cygwin/bin/bash")
;; (defun cygwin-shell ()
;;   "Run cygwin bash in shell mode."
;;   (interactive)
;;   (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
;;     (call-interactively 'shell)))

;;;
;;; use bash shell on windows
;;;
;; (setenv "PATH" (concat "c:/cygwin/bin") (getenv "PATH"))
;; (setq exec-path (append '("c:/cygwin/bin") exec-path))

;; (defun cygwin-shell ()
;;   "Run cygwin bash in shell mode."
;;   (interactive)
;;   (let ((explicit-shell-file-name "C:/cygwin/bin/bash.exe"))
;;     (call-interactively 'shell)))

;; (defun ffailla-shell-setup ()
;;   "For Cygwin bash under Emacs 20"
;;   (setq comint-scroll-show-maximum-output 'this)
;;   (make-variable-buffer-local 'comint-completion-addsuffix))
;; (setq comint-completion-addsuffix t)
;; (setq comint-eol-on-send t)
;; (setq binary-process-input t) 
;; (setq w32-quote-process-args ?\") 
;; (setq shell-file-name "bash") ;; or sh if you rename your bash executable to sh. 
;; (setenv "SHELL" shell-file-name) 
;; (setq explicit-shell-file-name shell-file-name) 
;; (setq explicit-sh-args '("-login" "-i"))
;; (setq comint-completion-addsuffix t)
;; (setq comint-eol-on-send t)

;; (setq shell-mode-hook 'ffailla-shell-setup)


;; emacs key bindings
;;; xml
(defalias 'ppx 'pprint-xml)

;;; misc
(defalias 'ttl 'toggle-truncate-lines)
(defalias 'rnb 'rename-buffer)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; slime
(defalias 'slc 'slime-connect)

;;; ido
(global-set-key (kbd "C-x C-i") 'ido-imenu)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; window layouts
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

;;; magit
(defalias 'ms 'magit-status)

;;; zoom in/out
;;(global-set-key [(control shift ?z)] 'text-scale-increase)
;;(global-set-key [(control ?z)]       'text-scale-decrease)

;;; alternate M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;;; tmux ?!?!?!?
(global-set-key (kbd " [C") 'paredit-forward-slurp-sexp)
;;(global-set-key (kbd "S-<left>") 'windmove-left)
;;(global-set-key (kbd "S-<up>") 'windmove-up)
;;(global-set-key (kbd "S-<down>") 'windmove-down)

;;;
;;; printing support
;;;
(require 'printing)
(require 'ps2pdf)
(pr-update-menus)
(setq ps-printer-name "PDF_file_generator")
(setq ps-printer-name t)

(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (message (pwd))
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf")))

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

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; (defvar ido-enable-replace-completing-read t
;;   "If t, use ido-completing-read instead of completing-read if possible.
    
;;     Set it to nil using let in around-advice for functions where the
;;     original completing-read is required.  For example, if a function
;;     foo absolutely must use the original completing-read, define some
;;     advice like this:
    
;;     (defadvice foo (around original-completing-read-only activate)
;;       (let (ido-enable-replace-completing-read) ad-do-it))")

;; ;; Replace completing-read wherever possible, unless directed otherwise
;; (defadvice completing-read
;;   (around use-ido-when-possible activate)
;;   (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
;; 	  (and (boundp 'ido-cur-list)
;; 	       ido-cur-list)) ; Avoid infinite loop from ido calling this
;;       ad-do-it
;;     (let ((allcomp (all-completions "" collection predicate)))
;;       (if allcomp
;; 	  (setq ad-return-value
;; 		(ido-completing-read prompt
;; 				     allcomp
;; 				     nil require-match initial-input hist def))
;; 	ad-do-it))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

;;;
;;; default font
;;;
(if (not (eq system-type 'windows-nt))
  (set-default-font "-unknown-Inconsolata-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))

;;;
;;; rainbow-delimiters
;;;
(require 'rainbow-delimiters)

;;;
;;; color-theme
;;;  * http://www.nongnu.org/color-theme/
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/ffailla/color-theme/")
(require 'color-theme)

(eval-after-load 'color-theme
  '(progn
     
     ;; https://github.com/bbatsov/zenburn-emacs
     (add-to-list 'load-path "~/.emacs.d/vendor/color-theme/themes/zenburn-emacs")  ; ensure zenburns's version of color-theme loaded
     (require 'color-theme-zenburn)
     (color-theme-zenburn)

     ;; (eval-after-load 'term
     ;;   '(setq ansi-term-color-vector
     ;; 	      (vector 'unspecified 
     ;; 		      zenburn-bg
     ;; 		      zenburn-red 
     ;; 		      zenburn-green
     ;; 		      zenburn-bg ;zenburn-yellow 
     ;; 		      zenburn-blue+1
     ;; 		      zenburn-magenta 
     ;; 		      zenburn-cyan
     ;; 		      ;; dirty fix
     ;; 		      "white")))

     ;;(color-theme-initialize)
     
     ;;(require 'calm-forest-rainbow)
     ;;(color-theme-calm-forest-rainbow)
     ;;(require 'zenburn-rainbow)
     ;;(color-theme-zenburn-rainbow)     
     ;;(require 'subtle-hacker-rainbow)
     ;;(color-theme-subtle-hacker-rainbow)

     ;; white bg
     ;;(color-theme-feng-shui) 
     ;;(color-theme-katester)
     ;;(color-theme-snowish)
     ;;(color-theme-wheat)
     ;;(color-theme-jsc-light)

     ;;(require 'blippblopp-rainbow)
     ;;(color-theme-blippblopp-rainbow)
     ;;(color-theme-blippblopp)

     ;;(color-theme-subtle-hacker-rainbow)
     ;;(color-theme-clarity-rainbow)
     ;;(color-theme-zenburn-rainbow)
     ;;(color-theme-hober)
     ;;(color-theme-classic)
     ;;(color-theme-comidia)
     ;;(color-theme-subtle-hacker)
     ;;(color-theme-matrix)
     ;;(color-theme-emacs-nw)
     ;;(color-theme-jsc-dark)
     ))

;;;
;;; diff-mode customization
;;;
(add-to-list 'auto-mode-alist '(".*_EDITMSG\\'" . diff-mode))
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
;;; markdown mode
;;;  * git://jblevins.org/git/markdown-mode.git
;;;
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (append '(("\\.text" . markdown-mode)
		("\\.md" . markdown-mode)
		("\\.mdwn" . markdown-mode) 
		("\\.mdt" . markdown-mode))
	      auto-mode-alist))

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
       (shell-quote-argument (buffer-file-name)))))

(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C-c C-p") 'markdown-preview-file))

;;;
;;; erc settings
;;;
(load "~/.ercpass" t)
(require 'erc-services)
(erc-services-mode 1)
;; (setq erc-prompt-for-nickserv-password nil)
;; (setq erc-nickserv-passwords
;;       `((freenode (("ffailla" . ,freenode-ffailla-pass)))))

;;(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
(setq erc-beep-match-types '(current-nick keyword pal))
(setq erc-fill-column 115)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE"
		      ;;"324" "329" "332" "333" "353" "477"
))

(setq erc-pals '("ethorsen1" "ethorsen" "thickey" "dkapsalis" "bstephenson" "pwade" "jstonier" "rhickey" "pairuser" "ffmacpro"))
(setq erc-enable-logging t)
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part t)

;; (setq apscript (format "
;;     set cusrPath to (path to \"cusr\" as string)
;;     set soundAlias to (cusrPath & \"bin:campfire-incoming.mp3\") as alias
;;     tell application \"Play Sound\"
;; 	play (soundAlias as alias)
;;     end tell
;;     "))

;; FF - problems in iterm via ssh/screen
;; (add-hook 'erc-text-matched-hook
;; 	  (lambda (match-type nickuserhost message)
;; 	    (do-applescript apscript)))

;;;
;;; magit
;;;  * http://github.com/philjackson/magit
;;;
(autoload 'magit-status "magit" nil t)

;;;
;;; mo-git-blame
;;;  * https://github.com/mbunkus/mo-git-blame.git
;;;
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;;;
;;; nxml-mode
;;;  * http://www.thaiopensource.com/nxml-mode/
;;;
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
;;; log4j mode
;;;  * http://log4j-mode.sourceforge.net/
;;;
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
;;(add-hook 'log4j-mode-hook (lambda () (linum-mode nil)))

(provide 'ffailla-emacs)


;;;
;;; sql-mode
;;;
;;(sql-set-product 'ms)

