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

(setenv "PATH" (concat "~/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/sbin:" (getenv "PATH")))
(setq exec-path (append '("~/bin" "/opt/local/bin" "/opt/local/sbin" "/usr/local/bin" "/sbin") exec-path))

;;;
;;; package manager
;;;
(add-to-list 'load-path "~/.emacs.d/elpa/")
(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'flymake)

;;;
;;; ffailla requires
;;;
(require 'ffailla-emacs)
(require 'ffailla-erc)
(require 'ffailla-prolog)
(require 'ffailla-tramp)
(require 'ffailla-lisp)
(require 'ffailla-objc)
(require 'ffailla-org)
(require 'ffailla-csharp)
(require 'ffailla-ess)
(require 'ffailla-sql)
(require 'ffailla-vc)
(require 'ffailla-xml)
(require 'ffailla-js)
(require 'ffailla-log4j)

;;;
;;; start emacs server
;;;  * use /Applications/Emacs.app/Contents/MacOS/bin/emacsclient as editor 
;;;
(add-hook 'after-init-hook 'server-start)
;; (if (not (and (boundp 'server-process)
;;               (memq (process-status server-process) '(connect listen open run))))
;;     (server-start))
(setq ns-pop-up-frames nil)

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                                     (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

