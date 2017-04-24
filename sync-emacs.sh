#!/bin/sh

if [ "$1" == "--restore" ] 
then  
  rsync -av --delete ./.emacs.d/ ~/.emacs.d
  exit 0;
fi

rsync -av --copy-dirlinks --delete ~/.emacs.d/ ./.emacs.d

