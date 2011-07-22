(defvar *emacs-load-start* (current-time))

(defvar dotfiles-dir "~/.emacs.d/" "The root Emacs Lisp source folder")
(defvar ext-dir (concat dotfiles-dir "vendor/") "The root folder for external packages")

(defun add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; add everything to the load path, then add the first level subfolders automatically
(add-to-list 'load-path dotfiles-dir)
(add-subfolders-to-load-path dotfiles-dir)
(add-subfolders-to-load-path ext-dir)

(setenv "PATH" (concat "~/bin:/usr/local/bin:/sbin:/opt/local/bin:/opt/local/sbin:" (getenv "PATH")))
(setq exec-path (append '("~/bin" "/usr/local/bin" "/sbin" "/opt/local/bin" "/opt/local/sbin") exec-path))

;;;
;;; use bash shell on windows
;;;
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
;;; package manager
;;;
(add-to-list 'load-path "~/.emacs.d/elpa/")
(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;;
;;; ffailla requires
;;;
(require 'ffailla-misc)
(require 'ffailla-color-theme)
(require 'ffailla-bindings)
(require 'ffailla-defuns)
(require 'ffailla-erc)
(require 'ffailla-prolog)
(require 'ffailla-tramp)
(require 'ffailla-lisp)
(require 'ffailla-objc)
(require 'ffailla-java)
(require 'ffailla-org)
(require 'ffailla-csharp)
(require 'ffailla-ess)
(require 'ffailla-sql)
(require 'ffailla-vc)
(require 'ffailla-xml)
(require 'ffailla-js)
(require 'ffailla-jira)
(require 'ffailla-log4j)
(require 'ffailla-ecb)

;;;
;;; start emacs server
;;;  * use /Applications/Emacs.app/Contents/MacOS/bin/emacsclient as editor for git
;;;
(if (not (and (boundp 'server-process)
              (memq (process-status server-process) '(connect listen open run))))
    (server-start))

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                                     (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
(put 'downcase-region 'disabled nil)
