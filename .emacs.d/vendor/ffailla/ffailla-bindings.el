;;; xml
(defalias 'ppx 'pprint-xml)

;;; misc
(defalias 'ttl 'toggle-truncate-lines)
(defalias 'rnb 'rename-buffer)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; slime
(defalias 'slc 'slime-connect)

;;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(defalias 'omps 'org-mobile-push)
(defalias 'ompl 'org-mobile-pull)

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

;; zoom in/out
;;(global-set-key [(control shift ?z)] 'text-scale-increase)
;;(global-set-key [(control ?z)]       'text-scale-decrease)

;; alternate M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; tmux ?!?!?!?
(global-set-key (kbd " [C") 'paredit-forward-slurp-sexp)
;;(global-set-key (kbd "S-<left>") 'windmove-left)
;;(global-set-key (kbd "S-<up>") 'windmove-up)
;;(global-set-key (kbd "S-<down>") 'windmove-down)

(provide 'ffailla-bindings)