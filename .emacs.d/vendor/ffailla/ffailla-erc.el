;;
;; erc settings
;;
(load "~/.ercpass" t)
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode (("ffailla" . ,freenode-ffailla-pass)))))

;;(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
(setq erc-beep-match-types '(current-nick keyword pal))
(setq erc-fill-column 115)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE"
		      ;;"324" "329" "332" "333" "353" "477"
))

(setq erc-pals '("ethorsen1" "ethorsen" "thickey" "dkapsalis" "bstephenson" "pwade" "jstonier" "rhickey" "pairuser" "ffmacpro"))
(setq erc-enable-logging t)
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part t)

(setq apscript (format "
    set cusrPath to (path to \"cusr\" as string)
    set soundAlias to (cusrPath & \"bin:campfire-incoming.mp3\") as alias
    tell application \"Play Sound\"
	play (soundAlias as alias)
    end tell
    "))

(add-hook 'erc-text-matched-hook
	  (lambda (match-type nickuserhost message)
	    (do-applescript apscript)))

(provide 'ffailla-erc)