;;;
;;; jira
;;;  * http://www.emacswiki.org/emacs/JiraMode
;;;  * http://www.emacswiki.org/emacs/jira.el
;;;
(defun jira-set-url ()
  (interactive)
  ;; "http://thortech.jira.com/rpc/xmlrpc"
  (setq jira-url (read-from-minibuffer "jira-url: ")))

(defun jira-start ()
  (interactive)
  (require 'jira))


(provide 'ffailla-jira)