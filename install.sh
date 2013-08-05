#!/bin/sh
echo "Installing configs for $(hostname)";

if [  ! -L ~/.bashrc ]; then
    mv -f ~/.bashrc ~/.bashrc.org > /dev/null 2>&1;
fi 
mv -f ~/.bash_profile ~/.bash_profile.org > /dev/null 2>&1;
ln -s ~/github/dot.config/bash/.bashrc ~/.bashrc;
ln -s ~/github/dot.config/bash/.bash_profile ~/.bash_profile;

touch ~/.bash_profile.priv;
touch ~/.bashrc.priv;

mv -f ~/.emacs ~/.emacs.org > /dev/null 2>&1;
mv -f ~/.emacs.d ~/.emacs.org > /dev/null 2>&1;
ln -s ~/github/dot.config/emacs.d/dot.emacs.el ~/.emacs;
ln -s ~/github/dot.config/emacs.d/ ~/.emacs.d;

echo "Please relogin for configurations to be reloaded.";
exit 0;
