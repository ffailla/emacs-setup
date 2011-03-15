#!/bin/sh

set p=`pwd`

if [ "$1" == "--restore" ] 
then  
  cd ./.emacs.d/site-lisp/cdt/ && lein deps install
  cd $p && ant -f ./.emacs.d/site-lisp/jisql/build.xml
  cd $p && cp ./.emacs ~/
  cd $p && rsync -av --delete ./.emacs.d/ ~/.emacs.d
  cd $p && rsync -av --delete ./bin/ ~/bin
  cd $p && cp ./.screenrc ~/
  exit 0;
fi

rsync -av --copy-dirlinks --delete ~/.emacs ./
rsync -av --copy-dirlinks --delete ~/.emacs.d/ ./.emacs.d
rsync -av --copy-dirlinks --delete ~/bin/ ./bin
rsync -av --copy-dirlinks --delete ~/.screenrc ./
