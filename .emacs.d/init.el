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

(defvar ffailla/packages '(ac-slime
			   auto-complete
			   autopair
			   clojure-mode
			   coffee-mode
			   csharp-mode
			   erlang
			   flycheck
			   go-autocomplete
			   go-eldoc
			   go-mode
			   graphviz-dot-mode
			   haskell-mode
			   htmlize
			   markdown-mode
			   marmalade
			   nodejs-repl
			   org
			   paredit
			   php-mode
			   rvm
			   smex
			   solarized-theme
			   writegood-mode
			   yaml-mode
			   ess
			   cider
			   pbcopy
			   python
			   auto-virtualenv
			   powershell
			   graphviz-dot-mode
			   go-mode
			   nhexl-mode
			   )
  "default packages")

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

;;; vendor dir
(defvar ffailla/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path ffailla/vendor-dir)

(dolist (project (directory-files ffailla/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

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
(setq column-number-mode t)
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
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f")   'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

;; (setq-default show-trailing-whitespace t)

;;; flyspell
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
;; (setq markdown-command "pandoc --smart -f markdown -t html")
;; (setq markdown-css-paths `(,(expand-file-name "markdown.css" ffailla/vendor-dir)))

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

(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "^"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;;; clojure
(add-hook 'clojure-mode-hook #'paredit-mode)

;;; pbcopy
; (require 'pbcopy)
(turn-on-pbcopy)

;;; xml pretty printer
;;;  * http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
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

;;; graphviz
;;(setq graphviz-dot-view-command "dot -o $1.png -Tpng $1 && open $1.png")


;;; Python
(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
(add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (nhexl-mode yaml-mode writegood-mode solarized-theme smex rvm powershell php-mode pbcopy paredit nodejs-repl marmalade markdown-mode htmlize haskell-mode graphviz-dot-mode go-eldoc go-autocomplete flycheck ess erlang csharp-mode coffee-mode cider autopair auto-virtualenv ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
