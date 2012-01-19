;;;
;;; jira
;;;  * http://www.emacswiki.org/emacs/JiraMode
;;;  * http://www.emacswiki.org/emacs/jira.el
;;;

;; (defun jira-set-url ()
;;   (interactive)
;;   ;; "http://thortech.jira.com/rpc/xmlrpc"
;;   (setq jira-url (read-from-minibuffer "jira-url: ")))

;; (defun jira-start ()
;;   (interactive)
;;   (require 'jira))

;;;
;;; org-jira
;;;  * http://www.emacswiki.org/emacs/OrgJiraMode
;;;  * https://github.com/baohaojun/org-jira
;;;

;; (defun jira-set-url ()
;;   (interactive)
;;   ;; "http://thortech.jira.com"
;;   (setq jira2-url (read-from-minibuffer "jira2-url: ")))

;; (defun jira-start ()
;;   (interactive)
;;   (require 'jira2)
;;   (require 'org-jira))

(setq jira2-url "http://thortech.jira.com")
(require 'jira2)
(require 'org-jira)

(provide 'ffailla-jira)