#!/bin/sh

if [ $1 == "--restore" ] 
then  
  cp ./.emacs ~/
  rsync -av --delete ./.emacs.d/ ~/.emacs.d
  rsync -av --delete ./bin/ ~/bin
  cp ./.screenrc ~/  
  cp ./.terminfo ~/
  ant -f ./.emacs.d/vendor/jisql/build.xml
  cd ./.emacs.d/vendor/cdt/ && lein deps install
  exit 0;
fi

rsync -av --copy-dirlinks --delete ~/.emacs ./
rsync -av --copy-dirlinks --delete ~/.emacs.d/ ./.emacs.d
rsync -av --copy-dirlinks --delete ~/bin/ ./bin
rsync -av --copy-dirlinks --delete ~/.screenrc ./
rsync -av --copy-dirlinks --delete ~/.terminfo ./
