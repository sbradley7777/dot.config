#!/bin/sh
# If the files are not symbolic links then they will be backed up. If the files
# are symbolic links then they will be removed and readded so that paths are
# correct.

echo "Installing configs for $(hostname)";

if [  ! -L ~/.bash_profile ]; then
    mv -f ~/.bash_profile ~/.bash_profile.org > /dev/null 2>&1;
else
    rm -f ~/.bash_profile
fi
ln -s ~/github/dot.config/bash/.bash_profile ~/.bash_profile;

if [  ! -L ~/.bashrc ]; then
    mv -f ~/.bashrc ~/.bashrc.org > /dev/null 2>&1;
else
    rm -f ~/.bashrc
fi
ln -s ~/github/dot.config/bash/.bashrc ~/.bashrc;

touch ~/.bash_profile.priv;
touch ~/.bashrc.priv;

if [  ! -L ~/.emacs ]; then
    mv -f ~/.emacs ~/.emacs.org > /dev/null 2>&1;
else
    rm -f ~/.emacs
fi
ln -s ~/github/dot.config/emacs.d/dot.emacs.el ~/.emacs;

if [  ! -L ~/.emacs.d ]; then
    mv -f ~/.emacs.d ~/.emacs.org > /dev/null 2>&1;
else
    rm -f ~/.emacs.d
fi
ln -s ~/github/dot.config/emacs.d/ ~/.emacs.d;

echo "Please relogin for configurations to be reloaded.";
exit 0;
