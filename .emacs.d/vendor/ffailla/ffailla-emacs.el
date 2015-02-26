;;; package --- ffailla-emacs.el
;;; Commentary:
;;; Code:

(require 'imenu)
(require 'recentf)

;;;
;;; ido
;;;
(require 'ido)
(require 'ido-ubiquitous)
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point t
        ido-max-prospects 10))

;;;
;;; flycheck
;;;
(require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;;;
;;; smex
;;;
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;set the title bar to display the full path of the buffer
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b" ("%b - " default-directory)))))))

;;;
;;; yasnippet
;;;
(require 'yasnippet)
;; (yas-global-mode 1)

;;;
;;; global settings
;;;
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
;; (setq ns-pop-up-frames nil)

(put 'downcase-region 'disabled nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

;;;
;;; windows specific configuration
;;;
;; (if (eq system-type 'windows-nt)
;;   (setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
;;   (setq exec-path (cons "c:/cygwin/bin/" exec-path))
;;   (require 'cygwin-mount)
;;   (cygwin-mount-activate)

;;   (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
;;   (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)
;;   (setq explicit-shell-file-name "bash.exe")
;;   ;; For subprocesses invoked via the shell
;;   ;; (e.g., "shell -c command")
;;   (setq shell-file-name explicit-shell-file-name)
  
;;   )

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

;;;
;;; linum settings
;;;
(require 'hlinum)
(when (fboundp 'fringe-mode) (fringe-mode 0))
(global-linum-mode 1)
(setq linum-format "%d ")
(hlinum-activate)

;;;
;;; desktop
;;;
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

(setq echo-keystrokes 0.1
      ;; use-dialog-box nil ; FF enabled for flyspell
      visible-bell t)
(show-paren-mode t)

;;(setq visible-bell f)
;;(setq ring-bell-function 'ignore)
;; (setq ring-bell-function
;;       (lambda ()
;;         (unless (memq this-command
;;                       '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit
;; 				      mwheel-scroll down up next-line previous-line
;; 				      backward-char forward-char))
;;           (ding))))

(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)

;;;
;;; autosave/backup tmp file locations
;;;
;; Put autosave files (ie #foo#) in one place, *not* scattered all over the file system!
(defvar autosave-dir (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
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
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)

;; emacs key bindings
(defalias 'ttl 'toggle-truncate-lines)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; alternate M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;;; tmux ?!?!?!?
;;(global-set-key (kbd " [C") 'paredit-forward-slurp-sexp)
;;(global-set-key (kbd "S-<left>") 'windmove-left)
;;(global-set-key (kbd "S-<up>") 'windmove-up)
;;(global-set-key (kbd "S-<down>") 'windmove-down)

;;;
;;; printing support
;;;
(require 'printing)

;;;
;;; Emacs Starter Kit fns
;;;
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

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;;; ido
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f")   'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
;;;
(require 'color-theme)

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/zenburn-emacs/")
(eval-after-load 'color-theme
  '(progn
     (load-theme 'zenburn)))

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
;;; erc settings
;;;
(require 'erc)

;;;
;;; magit
;;;  * http://github.com/philjackson/magit
;;;
(autoload 'magit-status "magit" nil t)
(defalias 'ms 'magit-status)

;;;
;;; nxml-mode
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

;;; log4j mode
;;;
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
;;(add-hook 'log4j-mode-hook (lambda () (linum-mode nil)))

;;;
;;; sql-mode
;;;
;; (sql-set-product 'ms)

;;;
;;; org-mode
;;;
(setq load-path (cons "~/.emacs.d/vendor/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/vendor/org-mode/contrib/lisp" load-path))
(require 'org-install)  ; org-install.el only has autoloads
(add-hook 'org-mode-hook 'turn-on-font-lock)    ; Org buffers only
;;(global-font-lock-mode 1)                     ; for all buffers

(if (not (file-exists-p "~/org")) (make-directory "~/org"))
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;;(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-directory "~/org/mobile")
(setq org-default-notes-file "~/org/notes.org")
(setq org-agenda-files (directory-files "~/org" t ".org$"))
(setq org-mobile-files org-agenda-files)

;; org-mode babel
(require 'ob)
(require 'ob-tangle)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (R . t)))

(setq org-babel-clojure-backend 'nrepl)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate nil)
(setq org-src-window-setup 'current-window)
;;(setq inferior-lisp-program "lein repl")
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

;; webdav rsync command
(setq org-webdav-username nil)
(setq org-webdav-server nil)
(setq org-webdav-directory "~/org")
;; override with my settings
(load "~/.mobile-org.el" t)

(modify-coding-system-alist 'file "\\.org\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.dat\\'" 'utf-8)

(defun org-set-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (directory-files "~/org" t ".org$")))

(defun org-set-org-mobile-files ()
  (interactive)
  (setq org-mobile-files org-agenda-files))

(defun org-sync-push ()
  (interactive)
  (org-mobile-push)  
  ;; (shell-command (format "rsync -e ssh -avzuP %s %s@%s:%s" 
  ;; 			 org-mobile-directory 
  ;; 			 org-webdav-username 
  ;; 			 org-webdav-server 
  ;; 			 org-webdav-directory))

  (start-process "mobile-org-rsync" 
  		 (get-buffer-create "*rsync-buffer*") 
  		 "rsync" "--verbose" "-e" "ssh" "-avzuP"
  		 (expand-file-name org-mobile-directory)
  		 (format "%s@%s:%s" 
  			 org-webdav-username 
  			 org-webdav-server 
  			 org-webdav-directory))
  )

(defun org-sync-pull ()
  (interactive)
  (shell-command (format "rsync --verbose -e ssh -avzuP %s@%s:%s/mobile %s/.." 
  			 org-webdav-username 
  			 org-webdav-server 
  			 org-webdav-directory
  			 (expand-file-name org-mobile-directory)))

  ;; (start-process "mobile-org-rsync" 
  ;; 		 (get-buffer-create "*rsync-buffer*") 
  ;; 		 "rsync" "--verbose" "-e" "ssh" "-avzuP"
  ;; 		 (format "%s@%s:%s/mobile" 
  ;; 			 org-webdav-username 
  ;; 			 org-webdav-server 
  ;; 			 org-webdav-directory)
  ;; 		 (format "%s/.." (expand-file-name org-mobile-directory)))

  (org-mobile-pull)
)

;;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(defalias 'omps 'org-sync-push)
(defalias 'ompl 'org-sync-pull)

;;; 
;;; evernote-mode
;;;  * http://code.google.com/p/emacs-evernote-mode/
;;;
;; (setq evernote-ruby-command "/opt/local/bin/ruby")
;; (require 'evernote-mode) 
;; (setq evernote-username "ffailla") ; optional: you can use this username as default.
;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
;;?? (add-hook 'evernote-mode-hook (function (lambda () (org-mode))))

;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)

;;;
;; FF - problems with screen
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)

;;(define-key global-map "\C-cc" 'org-capture)

;;;
;;; vlfi - large file support
;;;
(require 'vlf)

;;;
;;; pbcopy - copy paste support for emacs in terminal on osx
;;;
(require 'pbcopy)
(turn-on-pbcopy)

(provide 'ffailla-emacs)

;;; ffailla-emacs.el ends here
