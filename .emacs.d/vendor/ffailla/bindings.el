;;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;(global-set-key (kbd "C-x <up>") 'windmove-up)
;;(global-set-key (kbd "C-x <down>") 'windmove-down)
;;(global-set-key (kbd "C-x <right>") 'windmove-right)
;;(global-set-key (kbd "C-x <left>") 'windmove-left)

;;; ido
(global-set-key (kbd "C-x C-i") 'ido-imenu)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

