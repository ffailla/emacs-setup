#!/bin/sh

if [ "$1" == "--restore" ] 
then
  set p=`pwd`
  cd ./.emacs.d/site-lisp/cdt/ && lein deps install && cd $p 
  ant -f ./.emacs.d/site-lisp/jisql/build.xml
  rsync -av --delete ./.emacs ~/
  rsync -av --delete ./.emacs.d ~/
  rsync -av --delete ./bin ~/
  rsync -av --delete ./.screenrc ~/
  exit 0;
fi

rsync -av --copy-dirlinks --delete ~/.emacs ./
rsync -av --copy-dirlinks --delete ~/.emacs.d ./
rsync -av --copy-dirlinks --delete ~/bin ./
rsync -av --copy-dirlinks --delete ~/.screenrc ./
