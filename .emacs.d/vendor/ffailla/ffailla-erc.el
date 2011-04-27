;;
;; erc settings
;;
(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
(setq erc-beep-match-types '(current-nick keyword))
(setq erc-fill-column 100)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(provide 'ffailla-erc)