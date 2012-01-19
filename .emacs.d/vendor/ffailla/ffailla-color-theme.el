(if (not (eq system-type 'windows-nt))
  (set-default-font "-unknown-Inconsolata-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))

;; (set-face-attribute 'default (selected-frame) :height 100)
;; (defun maximize-frame ()
;;   (interactive)
;;   (set-frame-position (selected-frame) 0 0)
;;   (set-frame-size (selected-frame) 1000 1000))
;; (maximize-frame)


;; 
;; maxframe
;;  * https://github.com/rmm5t/maxframe.el.git
;;
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

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

;; FF hack to add this after color-theme require, so that zenburns's version
;;    of color-theme is not loaded
(add-to-list 'load-path "~/.emacs.d/vendor/color-theme/themes/zenburn-emacs")

(eval-after-load 'color-theme
  '(progn
     (color-theme-initialize)
     
     ;; https://github.com/bbatsov/zenburn-emacs
     ;;(require 'color-theme-zenburn)
     ;;(color-theme-zenburn)

     ;; (eval-after-load 'term
     ;;   '(setq ansi-term-color-vector
     ;; 	      (vector 'unspecified 
     ;; 		      zenburn-bg
     ;; 		      zenburn-red 
     ;; 		      zenburn-green
     ;; 		      zenburn-bg ;zenburn-yellow 
     ;; 		      zenburn-blue+1
     ;; 		      zenburn-magenta 
     ;; 		      zenburn-cyan
     ;; 		      ;; dirty fix
     ;; 		      "white")))

     (require 'calm-forest-rainbow)
     (color-theme-calm-forest-rainbow)
     ;;(require 'zenburn-rainbow)
     ;;(color-theme-zenburn-rainbow)
     
     ;;(require 'subtle-hacker-rainbow)
     ;;(color-theme-subtle-hacker-rainbow)

     ;; white bg
     ;;(color-theme-feng-shui) 
     ;;(color-theme-katester)
     ;;(color-theme-snowish)
     ;;(color-theme-wheat)
     ;;(color-theme-jsc-light)

     ;;(require 'blippblopp-rainbow)
     ;;(color-theme-blippblopp-rainbow)
     ;;(color-theme-blippblopp)

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