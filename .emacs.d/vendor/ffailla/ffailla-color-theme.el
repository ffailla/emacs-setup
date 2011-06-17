(set-default-font "-unknown-Inconsolata-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")

;;(set-face-attribute 'default (selected-frame) :height 100)
(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))
(maximize-frame)

;;;
;;; rainbow-delimeters
;;;
(require 'rainbow-delimiters)

;;;
;;; color-theme
;;;  * http://www.nongnu.org/color-theme/
;;;
(add-to-list 'load-path "~/.emacs.d/vendor/ffailla/color-theme/")
(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (require 'zenburn-rainbow)
     (color-theme-zenburn-rainbow)
     ;;(require 'subtle-hacker-rainbow)
     ;;(color-theme-subtle-hacker-rainbow)
     ;;(color-theme-clarity-rainbow)
     ;;(color-theme-zenburn-rainbow)
     ;;(color-theme-hober)
     ;;(color-theme-classic)
     ;;(color-theme-comidia)
     ;;(color-theme-subtle-hacker)
     ))

;;;
;;; diff-mode customization
;;;
(add-to-list 'auto-mode-alist '(".*_EDITMSG\\'" . diff-mode))
(custom-set-faces
 '(diff-added ((t (:foreground "#559944"))))
 '(diff-context ((t nil)))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-function ((t (:foreground "#00bbdd"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-hunk-header ((t (:foreground "#fbde2d"))))
 '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
 '(diff-removed ((t (:foreground "#de1923")))))

(provide 'ffailla-color-theme)