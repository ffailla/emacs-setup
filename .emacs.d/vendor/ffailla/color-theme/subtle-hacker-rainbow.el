(defun color-theme-subtle-hacker-rainbow ()
  "Subtle Hacker Color Theme.
Based on gnome2, but uses white for important things like comments,
and less of the unreadable tomato.  By Colin Walters <levanti@verbum.org>"
  (interactive)
  (color-theme-subtle-hacker)
  (let ((color-theme-is-cumulative t))
    (color-theme-install
     '(color-theme-subtle-hacker-rainbow
       nil
       nil

       (rainbow-delimiters-depth-1-face ((t (:bold t :foreground "royal blue"))))
       (rainbow-delimiters-depth-2-face ((t (:bold t :foreground "turquoise"))))
       (rainbow-delimiters-depth-3-face ((t (:bold t :foreground "tomato"))))
       (rainbow-delimiters-depth-4-face ((t (:bold t :foreground "dodger blue"))))
       (rainbow-delimiters-depth-5-face ((t (:bold t :foreground "green yellow"))))
       (rainbow-delimiters-depth-6-face ((t (:bold t :foreground "dark orange"))))
       (rainbow-delimiters-depth-7-face ((t (:bold t :foreground "slateblue1"))))
       (rainbow-delimiters-depth-8-face ((t (:bold t :foreground "salmon"))))
       (rainbow-delimiters-depth-9-face ((t (:bold t :foreground "burlywood"))))
       (rainbow-delimiters-depth-10-face ((t (:bold t :foreground "chocolate"))))
       (rainbow-delimiters-depth-11-face ((t (:bold t :foreground "light sea green"))))
       (rainbow-delimiters-depth-12-face ((t (:bold t :foreground "aquamarine"))))
       (rainbow-delimiters-depth-13-face ((t (:bold t :foreground "khaki"))))
       (rainbow-delimiters-depth-14-face ((t (:bold t :foreground "light sky blue"))))

       (rainbow-delimiters-unmatched-delimiter-face ((t (:bold t :foreground "darkgray"))))
       ))))

(provide 'subtle-hacker-rainbow)