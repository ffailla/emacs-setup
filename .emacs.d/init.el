;;; turn on debugging, turn off at end
(setq debug-on-error t)
(setq debug-on-quit t)

;;; user defaults
(setq user-full-name "Frank Failla")
(setq user-mail-address "frank@frankfailla.com")

;;; environment
(setenv "PATH" (concat "~/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/sbin:" (getenv "PATH")))
(setq exec-path (append '("~/bin" "/opt/local/bin" "/opt/local/sbin" "/usr/local/bin" "/sbin") exec-path))

;;; package management
(require 'cl-lib)
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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
			   magit
			   markdown-mode
			   marmalade
			   nodejs-repl
			   ;; org ; use downloaded version in vendor dir instead
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
			   nhexl-mode
			   vlf
			   ztree
			   ;; clj-refactor
			   )
  "default packages")

(defun ffailla/packages-installed-p ()
  (cl-loop for pkg in ffailla/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (ffailla/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ffailla/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;; helper fns
(defun slurp (f)
  (if (file-readable-p f)
      (with-temp-buffer
	(insert-file-contents f)
	(buffer-substring-no-properties
	 (point-min)
	 (point-max)))))

;;; vendor dir
(defvar ffailla/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path ffailla/vendor-dir)

(dolist (project (directory-files ffailla/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;;; startup settings
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      split-height-threshold nil
      split-width-threshold 200
      x-select-enable-clipboard t
      column-number-mode t
      tab-width 2
      indent-tabs-mode nil
      make-backup-files nil
      echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t
      confirm-nonexistent-file-or-buffer nil)

(setq-default indicate-empty-lines t)

(put 'list-timers 'disabled nil)

;; (scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode t)
(transient-mark-mode t)
(global-so-long-mode 1)
(show-paren-mode t)

(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(defalias 'yes-or-no-p 'y-or-n-p)

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
(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always)

(setq ido-file-extensions-order
      '(".org" ".txt" ".md"
	      ".clj" ".edn" ".cljs" ".cljc"
	      ".py" 
	      ".sh" ".bash"
	      ".emacs" ".el" ".lisp"
	      ".xml" ".yaml" ".yml"
	      ".ini" ".cfg" ".cnf"))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defadvice ido-switch-buffer (around no-confirmation activate)
  (let ((confirm-nonexistent-file-or-buffer nil))
    ad-do-it))

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f")   'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 40 40 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

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

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'text-mode-hook 'remove-dos-eol)
;; (setq-default show-trailing-whitespace t)

;;; flyspell
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;;; misc mode hooks
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;; js-mode
(add-hook 'coffee-mode-hook
	  (lambda ()
	    (make-local-variable 'tab-width)
	    (set 'tab-width 2)))
(add-hook 'js-mode-hook
	  (lambda ()
	    (setq js-indent-level 2)))

;;; markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))

(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C-c C-p") 'markdown-preview-file))

;;; colortheme
(if window-system
    (load-theme 'solarized-light t)
    (load-theme 'solarized-zenburn t)
    ;; (load-theme 'misterioso t) 
    ;; (load-theme 'wombat t)
    ;; (load-theme 'tsdh-dark t)
    ;; (load-theme 'cyberpunk t)
  )

;;; org-mode
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp")

;; (require 'org)
(require 'ob-clojure)
(setq org-directory "~/org"
      org-mobile-inbox-for-pull "~/org/flagged.org"
      org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; domain specific org files
(setq org-agenda-files
      (let ((fname "~/org/agenda-files.org"))
	(when (file-exists-p fname)
	  (split-string (slurp fname) "\n" t))))

(setq org-clock-persist 'history
      org-babel-clojure-sync-nrepl-timeout nil
      org-src-preserve-indentation t
      org-use-tag-inheritance nil
      org-confirm-babel-evaluate nil
      org-babel-clojure-backend 'cider
      org-startup-folded t     
      org-duration-format 'h:mm
      org-todo-keywords '((sequence "TODO(t!)" "INPROGRESS(i!)" "WAITING(w@)" "|" "DONE(d@)" "HOLD(h@)"))
      org-log-done t
      org-log-into-drawer t
      org-refile-use-outline-path 'file
      org-refile-targets '((org-agenda-files . (:maxlevel . 4)))
      org-outline-path-complete-in-steps nil
      org-agenda-clockreport-parameter-plist '(:maxlevel 5 :fileskip0 t :stepskip0 t :scope agenda :block thismonth :compact t :narrow 80))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	      ("INPROGRESS" :foreground "deep sky blue" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
        ("HOLD" :foreground "forest green" :weight bold)
        ))

(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t) 
			     (clojure . t)
			     (sql . t)
			     (shell . t)
			     (python . t)
			     (R . t)))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cs" 'org-insert-structure-template)

(org-clock-persistence-insinuate)

;; (run-with-idle-timer 360 360 'org-mobile-push)
;; (run-at-time "5 sec" 120 'org-mobile-push)

;; (defun my-org-clocktable-indent-string (level)
;;   (if (= level 1)
;;       ""
;;     (let ((str "^"))
;;       (while (> level 2)
;;         (setq level (1- level)
;;               str (concat str "--")))
;;       (concat str "-> "))))
;; (advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)
;; (add-hook 'org-src-mode-hook #'org-src-mode-configure-edit-buffer)

;;; clojure
;; (require 'clj-refactor)
(add-hook 'clojure-mode-hook
	  (lambda  ()
	    (paredit-mode 1)
	    (subword-mode t)
	    (yas-minor-mode 1) ; for adding require/use/import statements
	    (remove-dos-eol)
	    ;; (clj-refactor-mode 1)
	    ;; (cljr-add-keybindings-with-prefix "C-c C-m") ; This choice of keybinding leaves cider-macroexpand-1 unbound
	    ))
(add-hook 'cider-repl-mode-hook
	  (lambda ()
	    (setq scroll-conservatively 101
		  cider-history-file "~/.nrepl-history"
		  cider-hide-special-buffers t
		  cider-repl-history-size 10000
		  cider-prefer-local-resources t)
	    (paredit-mode 1)))
	  
(setq cider-allow-jack-in-without-project t
      cider-print-options '(("length" 500) ("level" 10))
      cider-clojure-cli-global-options "-A:cider-clj")

;; (setq cider-clojure-cli-parameters "-A:cider-clj -m nrepl.cmdline --middleware '%s'")

;;; datomic
;; Similar to C-x C-e, but sends to REBL
;; (defun rebl-eval-last-sexp ()
;;   (interactive)
;;   (let* ((bounds (cider-last-sexp 'bounds))
;;          (s (cider-last-sexp))
;;          (reblized (concat "(cognitect.rebl/inspect " s ")")))
;;     (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; ;; Similar to C-M-x, but sends to REBL
;; (defun rebl-eval-defun-at-point ()
;;   (interactive)
;;   (let* ((bounds (cider-defun-at-point 'bounds))
;;          (s (cider-defun-at-point))
;;          (reblized (concat "(cognitect.rebl/inspect " s ")")))
;;     (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; ;; C-S-x send defun to rebl
;; ;; C-x C-r send last sexp to rebl (Normally bound to "find-file-read-only"... Who actually uses that though?)
;; (add-hook 'cider-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-S-x") #'rebl-eval-defun-at-point)
;;             (local-set-key (kbd "C-x C-r") #'rebl-eval-last-sexp)))

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
;; (setq graphviz-dot-view-command "dot -o $1.svg -Tsvg $1 && open $1.svg")

;;; Python
(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
(add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)

;; (use-package pyenv-mode
;;   :init
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (setenv "WORKON_HOME" "~/.pyenv/versions/")
;;   :config
;;   (pyenv-mode)
;;   :bind
;;   ("C-x p e" . pyenv-activate-current-project))

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;;; ediff
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ediff-current-diff-A ((t (:background "black"))))
;;  '(ediff-current-diff-B ((t (:background "black"))))
;;  '(ediff-current-diff-C ((t (:background "black"))))
;;  '(ediff-even-diff-A ((t (:inherit hl-line))))
;;  '(ediff-even-diff-B ((t (:inherit hl-line))))
;;  '(ediff-even-diff-C ((t (:inherit hl-line))))
;;  '(ediff-fine-diff-A ((t (:background "grey-d" :bold t))))
;;  '(ediff-fine-diff-B ((t (:background "grey-d" :bold t))))
;;  '(ediff-fine-diff-C ((t (:background "grey-d" :bold t))))
;;  '(ediff-odd-diff-A ((t (:inherit hl-line))))
;;  '(ediff-odd-diff-B ((t (:inherit hl-line))))
;;  '(ediff-odd-diff-C ((t (:inherit hl-line)))))

;;; turn off debugging end
(setq debug-on-error nil)
(setq debug-on-quit nil)

;;; custom-set-variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-gcal ztree yaml-mode writegood-mode vlf solarized-theme smex rvm pythonic powershell php-mode pbcopy nodejs-repl nhexl-mode marmalade markdown-mode magit julia-mode htmlize highlight-indentation haskell-mode graphviz-dot-mode go-eldoc go-autocomplete flycheck ess erlang csharp-mode company coffee-mode clj-refactor autopair auto-virtualenv ac-slime))
 '(safe-local-variable-values
   '((cider-figwheel-main-default-options . ":dev")
     (cider-default-cljs-repl . figwheel-main)
     (eval defun set-mt-staging nil
	   (interactive)
	   (setenv "AWS_PROFILE" "mt2-staging")
	   (setenv "MT2_ENV" "staging")
	   (setenv "MT2_CLUSTER" "staging")
	   (setenv "AWS_DEFAULT_REGION" "us-east-2"))
     (eval defun set-mt-prod nil
	   (interactive)
	   (setenv "AWS_PROFILE" "mt2-prod")
	   (setenv "MT2_ENV" "prod")
	   (setenv "MT2_CLUSTER" "prod")
	   (setenv "AWS_DEFAULT_REGION" "us-east-2"))
     (eval defun set-mt-dev nil
	   (interactive)
	   (setenv "AWS_PROFILE" "mt2-dev")
	   (setenv "MT2_ENV" "local")
	   (setenv "MT2_CLUSTER" "local")
	   (setenv "LOCAL_CUSTOMER_ID" "ffailla")
	   (setenv "AWS_DEFAULT_REGION" "us-east-2"))
     (eval defun set-mt-qa2 nil
	   (interactive)
	   (setenv "AWS_PROFILE" "mt2-qa2")
	   (setenv "MT2_ENV" "local")
	   (setenv "MT2_CLUSTER" "local")
	   (setenv "AWS_DEFAULT_REGION" "us-east-2"))
     (eval defun set-mt-qa nil
	   (interactive)
	   (setenv "AWS_PROFILE" "mt2-qa")
	   (setenv "MT2_ENV" "local")
	   (setenv "MT2_CLUSTER" "local")
	   (setenv "AWS_DEFAULT_REGION" "us-east-2"))
     (eval defun set-mt-prod nil
	   (interactive)
	   (setenv "AWS_PROFILE" "mt2-prod")
	   (setenv "MT2_ENV" "prod")
	   (setenv "MT2_CLUSTER" "prod")
	   (setenv "LOCAL_CUSTOMER_ID" "ffailla")
	   (setenv "AWS_DEFAULT_REGION" "us-east-2"))
     (eval defun set-mt-dev nil
	   (interactive)
	   (setenv "AWS_PROFILE" "mt2-dev")
	   (setenv "MT2_ENV" "local")
	   (setenv "MT2_CLUSTER" "local")
	   (setenv "LOCAL_CUSTOMER_ID" "ffailla")
	   (setenv "AWS_DEFAULT_REGION" "us-east-2")
	   (setenv "ES_BASE_URI" "http://localhost:9200")
	   (setenv "ES_AUTH_CREDENTIALS" "elastic:elastic"))
     (eval defun set-mt-qa2 nil
	   (interactive)
	   (setenv "AWS_PROFILE" "mt2-qa2")
	   (setenv "MT2_ENV" "local")
	   (setenv "MT2_CLUSTER" "local"))
     (setenv "AWS_DEFAULT_REGION" "us-east-1"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
