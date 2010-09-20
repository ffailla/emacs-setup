#!/bin/sh

if [ "$1" == "--restore" ] 
then
  rsync -av --copy-dirlinks --delete ./.emacs ~/.emacs
  rsync -av --copy-dirlinks --delete ./.emacs.d ~/.emacs.d
  exit 0;
fi

cd ~/dev/emacs-setup
rsync -av --copy-dirlinks --delete ~/.emacs ./
rsync -av --copy-dirlinks --delete ~/.emacs.d ./
