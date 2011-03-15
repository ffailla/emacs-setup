#!/bin/sh

if [ $1 == "--restore" ] 
then  
  set pw=`pwd`
  cd ./.emacs.d/site-lisp/cdt/ && lein deps install
  cd $pw && ant -f ./.emacs.d/site-lisp/jisql/build.xml
  cd $pw && cp ./.emacs ~/
  cd $pw && rsync -av --delete ./.emacs.d/ ~/.emacs.d
  cd $pw && rsync -av --delete ./bin/ ~/bin
  cd $pw && cp ./.screenrc ~/  
  exit 0;
fi

rsync -av --copy-dirlinks --delete ~/.emacs ./
rsync -av --copy-dirlinks --delete ~/.emacs.d/ ./.emacs.d
rsync -av --copy-dirlinks --delete ~/bin/ ./bin
rsync -av --copy-dirlinks --delete ~/.screenrc ./
