#!/bin/sh
echo "Installing configs for $(hostname)";

if [  ! -L ~/.bash_profile ]; then
    mv -f ~/.bash_profile ~/.bash_profile.org > /dev/null 2>&1;
    ln -s ~/github/dot.config/bash/.bash_profile ~/.bash_profile;
fi
if [  ! -L ~/.bashrc ]; then
    mv -f ~/.bashrc ~/.bashrc.org > /dev/null 2>&1;
    ln -s ~/github/dot.config/bash/.bashrc ~/.bashrc;
fi

touch ~/.bash_profile.priv;
touch ~/.bashrc.priv;

if [  ! -L ~/.emacs ]; then
    mv -f ~/.emacs ~/.emacs.org > /dev/null 2>&1;
    ln -s ~/github/dot.config/emacs.d/dot.emacs.el ~/.emacs;
fi

if [  ! -L ~/.emacs.d ]; then
    mv -f ~/.emacs.d ~/.emacs.org > /dev/null 2>&1;
    ln -s ~/github/dot.config/emacs.d/ ~/.emacs.d;
fi

echo "Please relogin for configurations to be reloaded.";
exit 0;
