;;;
;;; org-mode
;;;  * http://orgmode.org/
;;;
(setq load-path (cons "~/.emacs.d/vendor/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/vendor/org-mode/contrib/lisp" load-path))
(require 'org-install)  ; org-install.el only has autoloads
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
;;(global-font-lock-mode 1)                     ; for all buffers

(if (not (file-exists-p "~/org")) (make-directory "~/org"))
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-default-notes-file "~/org/notes.org")
(setq org-agenda-files (directory-files "~/org" t ".org$"))
;;(setq org-mobile-files (directory-files "~/org" t ".org$"))
(setq org-mobile-files org-agenda-files)
(define-key global-map "\C-cc" 'org-capture)

(modify-coding-system-alist 'file "\\.org\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.dat\\'" 'utf-8)

(defun org-set-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (directory-files "~/org" t ".org$")))

(defun org-set-org-mobile-files ()
  (interactive)
  (setq org-mobile-files org-agenda-files))

(provide 'ffailla-org)