;;;
;;; ess
;;;  * http://ess.r-project.org/
;;;
(setq ess-r-versions nil)

(autoload 'ess-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R "ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'Rnw-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'omegahat-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'XLS-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'STA-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'SAS-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-transcript-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-transcript-mode "ess-site" "Emacs Speaks Statistics" t)

(autoload 'actr-mode "actr-mode" "ACT-R mode" 'interactive nil)
;;(add-to-list (quote auto-mode-alist) (quote ("\\.actr\\'" . actr-mode)))

(setq auto-mode-alist
      (append
       '(("\\.sp\\'"    . S-mode) ;; re: Don MacQueen <macq@llnl.gov>
         ("\\.[qsS]\\'" . S-mode) ;; q,s,S [see ess-restore-asm-extns above!]
         ("\\.ssc\\'"   . S-mode) ;; Splus 4.x script files.
         ("\\.[rR]\\'"  . R-mode)
         ("\\.[rR]nw\\'"  . Rnw-mode)
         ("\\.[rR]profile\\'" . R-mode)
         ("NAMESPACE\\'"      . R-mode)
         ("\\.omg\\'"         . omegahat-mode)
         ("\\.hat\\'"         . omegahat-mode) ;; Duncan's pref'd...
         ("\\.lsp\\'"         . XLS-mode)
         ("\\.do\\'"          . STA-mode)
         ("\\.ado\\'"         . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'"  . SAS-mode)
         ;; Many .log/.lst files, not just SAS
         ;;("\\.log\\'" . SAS-log-mode)
         ;;("\\.lst\\'" . SAS-listing-mode)
         ("\\.[Ss]t\\'" . S-transcript-mode)
         ("\\.[Ss]out"  . S-transcript-mode)
         ("\\.[Rr]t\\'" . R-transcript-mode)
         ("\\.[Rr]out"  . R-transcript-mode)
         )
       auto-mode-alist))

(provide 'ffailla-ess)