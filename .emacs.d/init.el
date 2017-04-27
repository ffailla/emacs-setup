;;; user defaults
(setq user-full-name "Frank Failla")
(setq user-mail-address "frank@frankfailla.com")

;;; environment
(setenv "PATH" (concat "~/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/sbin:" (getenv "PATH")))
(setq exec-path (append '("~/bin" "/opt/local/bin" "/opt/local/sbin" "/usr/local/bin" "/sbin") exec-path))
(require 'cl)

;;; package management
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar ffailla/packages '(ac-slime
			   auto-complete
			   autopair
			   clojure-mode
			   coffee-mode
			   csharp-mode
			   ; deft
			   erlang
			   ; feature-mode
			   flycheck
			   ; gist
			   go-autocomplete
			   go-eldoc
			   go-mode
			   ; graphviz-dot-mode
			   ; haml-mode
			   haskell-mode
			   htmlize
			   ; idris-mode
			   ; magit
			   markdown-mode
			   marmalade
			   nodejs-repl
			   ; o-blog
			   org
			   paredit
			   php-mode
			   ; puppet-mode
			   ; restclient
			   rvm
			   ; scala-mode
			   smex
			   ; sml-mode
			   solarized-theme
			   ; web-mode
			   writegood-mode
			   yaml-mode
			   ess
			   cider
			   pbcopy
			   python
			   ;;ido-ubiquitous
			   powershell
			   graphviz-dot-mode
			   go-mode
			   )
  "Default packages")

(defun ffailla/packages-installed-p ()
  (loop for pkg in ffailla/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (ffailla/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ffailla/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;; startup settings
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; (scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq tab-width 2 indent-tabs-mode nil)
(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

(defvar ffailla/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path ffailla/vendor-dir)

(dolist (project (directory-files ffailla/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;;; smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;; recent
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;; ido
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
;; (require 'ido-ubiquitous)
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f")   'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; column nummbers
(setq column-number-mode t)

;;; tmp files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;; autopair
(require 'autopair)

;;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;;; indentation and buffer
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

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;;; (setq-default show-trailing-whitespace t)

;; flyspell
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;;; mode hooks
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)

;;; markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
;;(setq markdown-command "pandoc --smart -f markdown -t html")
;;(setq markdown-css-paths `(,(expand-file-name "markdown.css" ffailla/vendor-dir)))

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
       (shell-quote-argument (buffer-file-name)))))

(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C-c C-p") 'markdown-preview-file))


;;; colortheme
(if window-system
    (load-theme 'solarized-light t)
    (load-theme 'wombat t))

;;; org-mode
;; (require 'org)
;; (require 'ob-clojure)
;; (setq org-babel-clojure-backend 'cider)
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode")
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;; cider


;;; pbcopy
; (require 'pbcopy)
(turn-on-pbcopy)

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
;;; graphviz
;;;
;;(setq graphviz-dot-view-command "dot -o $1.png -Tpng $1 && open $1.png")
