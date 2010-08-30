#!/bin/sh

cd ~/dev/emacs-setup
rsync -av --copy-dirlinks --delete ~/.emacs ./
rsync -av --copy-dirlinks --delete ~/.emacs.d ./

#cd ~/.emacs.d/site-lisp/
#ln -s ~/dev/ac-slime/ ac-slime
#ln -s ~/dev/auto-complete-1.3/ auto-complete
#ln -s ~/dev/cedet-1.0beta3b/ cedet
#ln -s ~/dev/clojure-mode/ clojure-mode
#ln -s ~/dev/elib-1.0/ elib
#ln -s ~/dev/emacs-nav/ emacs-nav
#ln -s ~/dev/emacs-highlight-parentheses/ highlight-parentheses
#ln -s ~/dev/emacs-javascript/ javascript
#ln -s ~/dev/jdee-2.4.0.1/ jdee
#ln -s ~/dev/log4j-mode/ log4j-mode
#ln -s ~/dev/magit/ magit
#ln -s ~/dev/nxml-mode-20041004/ nxml-mode
#ln -s ~/dev/org-mode/ org-mode
#ln -s ~/dev/paredit/ paredit
#ln -s ~/dev/save-visited-files/ save-visited-files
#ln -s ~/dev/slime-2010-08-18/ slime

#cd ~/.emacs.d/site-lisp/
#hunlink ac-slime
#hunlink auto-complete
#hunlink cedet
#hunlink clojure-mode
#hunlink elib
#hunlink emacs-nav
#hunlink highlight-parentheses
#hunlink javascript
#hunlink jdee
#hunlink log4j-mode
#hunlink magit
#hunlink nxml-mode
#hunlink org-mode
#hunlink paredit
#hunlink save-visited-files
#hunlink slime
#hlink ~/dev/ac-slime/ ac-slime
#hlink ~/dev/auto-complete-1.3/ auto-complete
#hlink ~/dev/cedet-1.0beta3b/ cedet
#hlink ~/dev/clojure-mode/ clojure-mode
#hlink ~/dev/elib-1.0/ elib
#hlink ~/dev/emacs-nav/ emacs-nav
#hlink ~/dev/emacs-highlight-parentheses/ highlight-parentheses
#hlink ~/dev/emacs-javascript/ javascript
#hlink ~/dev/jdee-2.4.0.1/ jdee
#hlink ~/dev/log4j-mode/ log4j-mode
#hlink ~/dev/magit/ magit
#hlink ~/dev/nxml-mode-20041004/ nxml-mode
#hlink ~/dev/org-mode/ org-mode
#hlink ~/dev/paredit/ paredit
#hlink ~/dev/save-visited-files/ save-visited-files
#hlink ~/dev/slime-2010-08-18/ slime