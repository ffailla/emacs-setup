#!/bin/sh

if [ "$1" == "--restore" ] 
then
  set p=pwd
  cd ./.emacs.d/site-lisp/cdt/ && lein deps, install && cd $p 
  ant -f ./.emacs.d/site-lisp/jisql/build.xml
  rsync -av --delete ./.emacs ~/
  rsync -av --delete ./.emacs.d ~/
  exit 0;
fi

cd ~/dev/emacs-setup
rsync -av --copy-dirlinks --delete ~/.emacs ./
rsync -av --copy-dirlinks --delete ~/.emacs.d ./
