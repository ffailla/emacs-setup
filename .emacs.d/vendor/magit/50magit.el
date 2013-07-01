;;; 50magit.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (magit-status) "magit" "magit.el" (20890 20678))
;;; Generated autoloads from magit.el

(autoload (quote magit-status) "magit" "\
Open a Magit status buffer for the Git repository containing
DIR.  If DIR is not within a Git repository, offer to create a
Git repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git control.
Two prefix arguments means to ignore `magit-repo-dirs' when asking for
user input.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (magit-blame-mode) "magit-blame" "magit-blame.el"
;;;;;;  (20890 20678))
;;; Generated autoloads from magit-blame.el

(autoload (quote magit-blame-mode) "magit-blame" "\
Display blame information inline.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (turn-on-magit-stgit magit-stgit-mode) "magit-stgit"
;;;;;;  "magit-stgit.el" (20890 20678))
;;; Generated autoloads from magit-stgit.el

(autoload (quote magit-stgit-mode) "magit-stgit" "\
StGit support for Magit

\(fn &optional ARG)" t nil)

(autoload (quote turn-on-magit-stgit) "magit-stgit" "\
Unconditionally turn on `magit-stgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-svn magit-svn-mode) "magit-svn"
;;;;;;  "magit-svn.el" (20890 20678))
;;; Generated autoloads from magit-svn.el

(autoload (quote magit-svn-mode) "magit-svn" "\
SVN support for Magit

\(fn &optional ARG)" t nil)

(autoload (quote turn-on-magit-svn) "magit-svn" "\
Unconditionally turn on `magit-svn-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-topgit magit-topgit-mode) "magit-topgit"
;;;;;;  "magit-topgit.el" (20890 20678))
;;; Generated autoloads from magit-topgit.el

(autoload (quote magit-topgit-mode) "magit-topgit" "\
Topgit support for Magit

\(fn &optional ARG)" t nil)

(autoload (quote turn-on-magit-topgit) "magit-topgit" "\
Unconditionally turn on `magit-topgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (global-magit-wip-save-mode magit-wip-save-mode
;;;;;;  magit-wip-mode) "magit-wip" "magit-wip.el" (20890 20678))
;;; Generated autoloads from magit-wip.el

(defvar magit-wip-mode nil "\
Non-nil if Magit-Wip mode is enabled.
See the command `magit-wip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.")

(custom-autoload (quote magit-wip-mode) "magit-wip" nil)

(autoload (quote magit-wip-mode) "magit-wip" "\
In Magit log buffers; give wip refs a special appearance.

\(fn &optional ARG)" t nil)

(autoload (quote magit-wip-save-mode) "magit-wip" "\
Magit support for committing to a work-in-progress ref.

When this minor mode is turned on and a file is saved inside a writable
git repository then it is also committed to a special work-in-progress
ref.

\(fn &optional ARG)" t nil)

(defvar global-magit-wip-save-mode nil "\
Non-nil if Global-Magit-Wip-Save mode is enabled.
See the command `global-magit-wip-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-wip-save-mode'.")

(custom-autoload (quote global-magit-wip-save-mode) "magit-wip" nil)

(autoload (quote global-magit-wip-save-mode) "magit-wip" "\
Toggle Magit-Wip-Save mode in every possible buffer.
With prefix ARG, turn Global-Magit-Wip-Save mode on if and only if ARG is positive.
Magit-Wip-Save mode is enabled in all buffers where `turn-on-magit-wip-save' would do it.
See `magit-wip-save-mode' for more information on Magit-Wip-Save mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rebase-mode) "rebase-mode" "rebase-mode.el" (20890
;;;;;;  20678))
;;; Generated autoloads from rebase-mode.el

(autoload (quote rebase-mode) "rebase-mode" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("git-rebase-todo" . rebase-mode)))

;;;***

;;;### (autoloads nil nil ("magit-bisect.el" "magit-key-mode.el"
;;;;;;  "magit-pkg.el") (20941 37020 501794))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; 50magit.el ends here
