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

;;;
;;; linum settings
;;;
;;(require 'linum)
(require 'hlinum)
(when (fboundp 'fringe-mode) (fringe-mode 0))
(global-linum-mode 1)
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

;;(setq fringe-mode (cons 4 0))
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

(provide 'ffailla-misc)
