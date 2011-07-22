(defun color-theme-blippblopp-rainbow ()
  "Color theme by Thomas Sicheritz-Ponten, created 2001-03-12.
Used by researchers at Uppsala University and the Center for Biological
Sequence Analysis at the Technical University of Denmark. (As some of my
swedish friends couldn't pronounce Sicheritz - they choose to transform
it to something more \"swedish\": Blippblopp :-)
Includes font-lock and message."
  (interactive)
  (color-theme-blippblopp)
  (let ((color-theme-is-cumulative t))
    (color-theme-install
     '(color-theme-blippblopp
       nil
       nil

       (org-level-1 ((t (:foreground "royal blue"))))
       (org-level-2 ((t (:foreground "dark red"))))
       (org-level-3 ((t (:foreground "slate blue"))))
       (org-level-4 ((t (:foreground "dark orange"))))
       (org-level-5 ((t (:foreground "purple"))))
       (org-level-6 ((t (:foreground "chocolate"))))
       (org-level-7 ((t (:foreground "dark green"))))
       (org-level-8 ((t (:foreground "red"))))

       ;; (erc-action ((t (:inherit erc-default))))
       ;; (erc-bold ((t (:weight bold))))
       ;; (erc-current-nick ((t (:inherit zenburn-primary-1))))
       ;; (erc-dangerous-host ((t (:inherit font-lock-warning))))
       ;; (erc-default ((t (:foreground ,zenburn-fg))))
       ;; (erc-direct-msg ((t (:inherit erc-default))))
       ;; (erc-error ((t (:inherit font-lock-warning))))
       ;; (erc-fool ((t (:inherit zenburn-lowlight-1))))
       ;; (erc-highlight ((t (:inherit hover-highlight))))
       ;; (erc-input ((t (:foreground ,zenburn-yellow))))
       ;; (erc-keyword ((t (:inherit zenburn-primary-1))))
       ;; (erc-nick-default ((t (:inherit bold))))
       ;; (erc-nick-msg ((t (:inherit erc-default))))
       ;; (erc-notice ((t (:inherit zenburn-green))))
       ;; (erc-pal ((t (:inherit zenburn-primary-3))))
       ;; (erc-prompt ((t (:inherit zenburn-primary-2))))
       ;; (erc-timestamp ((t (:inherit zenburn-green+1))))
       ;; (erc-underline ((t (:inherit underline))))

       ;; (magit-section-title ((t (:inherit zenburn-primary-1))))
       ;; (magit-branch ((t (:inherit zenburn-primary-2))))

       (rainbow-delimiters-depth-1-face ((t (:bold t :foreground "royal blue"))))
       (rainbow-delimiters-depth-2-face ((t (:bold t :foreground "turquoise"))))
       (rainbow-delimiters-depth-3-face ((t (:bold t :foreground "tomato"))))
       (rainbow-delimiters-depth-4-face ((t (:bold t :foreground "dodger blue"))))
       (rainbow-delimiters-depth-5-face ((t (:bold t :foreground "purple"))))
       (rainbow-delimiters-depth-6-face ((t (:bold t :foreground "dark orange"))))
       (rainbow-delimiters-depth-7-face ((t (:bold t :foreground "slateblue1"))))
       (rainbow-delimiters-depth-8-face ((t (:bold t :foreground "salmon"))))
       (rainbow-delimiters-depth-9-face ((t (:bold t :foreground "burlywood"))))
       (rainbow-delimiters-depth-10-face ((t (:bold t :foreground "chocolate"))))
       (rainbow-delimiters-depth-11-face ((t (:bold t :foreground "red"))))
       (rainbow-delimiters-depth-12-face ((t (:bold t :foreground "aquamarine"))))
       (rainbow-delimiters-depth-13-face ((t (:bold t :foreground "khaki"))))
       (rainbow-delimiters-depth-14-face ((t (:bold t :foreground "light sky blue"))))

       (rainbow-delimiters-unmatched-delimiter-face ((t (:bold t :foreground "darkgray"))))))))

(provide 'blippblopp-rainbow)