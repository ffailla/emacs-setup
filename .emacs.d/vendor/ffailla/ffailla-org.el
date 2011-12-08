;;;
;;; org-mode
;;;  * http://orgmode.org/
;;;
(setq load-path (cons "~/.emacs.d/vendor/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/vendor/org-mode/contrib/lisp" load-path))
(require 'org-install)  ; org-install.el only has autoloads
(add-hook 'org-mode-hook 'turn-on-font-lock)    ; Org buffers only
;;(global-font-lock-mode 1)                     ; for all buffers

(if (not (file-exists-p "~/org")) (make-directory "~/org"))
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-default-notes-file "~/org/notes.org")
(setq org-agenda-files (directory-files "~/org" t ".org$"))
(setq org-mobile-files org-agenda-files)

(modify-coding-system-alist 'file "\\.org\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.dat\\'" 'utf-8)

(defun org-set-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (directory-files "~/org" t ".org$")))

(defun org-set-org-mobile-files ()
  (interactive)
  (setq org-mobile-files org-agenda-files))

;;; 
;;; evernote-mode
;;;  * http://code.google.com/p/emacs-evernote-mode/
;;;
;;(require 'evernote-mode) 
;;(setq evernote-username "ffailla") ; optional: you can use this username as default.
;;(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
;;?? (add-hook 'evernote-mode-hook (function (lambda () (org-mode))))

;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)

;;(define-key global-map "\C-cc" 'org-capture)

(provide 'ffailla-org)