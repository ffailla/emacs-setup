#!/bin/sh

if [ "$1" == "--restore" ] 
then  
  git submodule init
  git submodule update
  rsync -av --delete ./.emacs.d/ ~/.emacs.d
  #rsync -av --delete ./bin/ ~/bin
  cp ./.screenrc ~/  
  exit 0;
fi

rsync -av --copy-dirlinks --delete ~/.emacs.d/ ./.emacs.d
#rsync -av --copy-dirlinks --delete ~/bin/ ./bin
rsync -av --copy-dirlinks --delete ~/.screenrc ./
