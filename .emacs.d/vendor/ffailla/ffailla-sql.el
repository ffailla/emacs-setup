;;;
;;; sql-mode / jisql
;;;  * http://www.xigole.com/software/jisql/jisql.jsp
;;;
(setq exec-path (append exec-path '("~/.emacs.d/vendor/jisql")))
(defun sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat "~/.emacs.d/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))
(add-hook 'sql-interactive-mode-hook 'sql-save-history-hook)

(provide 'ffailla-sql)