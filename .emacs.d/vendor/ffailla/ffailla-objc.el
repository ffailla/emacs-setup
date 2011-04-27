;;;
;;; objective-c / xcode
;;;  * find . \( -name "*.cpp" -o -name "*.h" -o -name "*.m" -o -name "*.mm" \) -print | etags -
;;;
(defun xcode-compile ()
  (interactive)
  (let ((df (directory-files "."))
        (has-proj-file nil))
    (while (and df (not has-proj-file))
      (let ((fn (car df)))
        (if (> (length fn) 10)
            (if (string-equal (substring fn -10) ".xcodeproj")
                (setq has-proj-file t))))
      (setq df (cdr df)))
    (if has-proj-file
        (compile "xcodebuild -configuration Debug")
      (compile "make"))))

(provide 'ffailla-objc)