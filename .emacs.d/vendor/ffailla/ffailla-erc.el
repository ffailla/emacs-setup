;;
;; erc settings
;;
(load "~/.ercpass")
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode (("ffailla" . ,freenode-ffailla-pass)))))

(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
(setq erc-beep-match-types '(current-nick keyword))
(setq erc-fill-column 100)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(provide 'ffailla-erc)