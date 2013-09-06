#!/bin/sh
# If the files are not symbolic links then they will be backed up. If the files
# are symbolic links then they will be removed and readded so that paths are
# correct.

# Create symbolic links for bash configuration.
if [  ! -L $HOME/.bash_profile ]; then
    mv -f $HOME/.bash_profile $HOME/.bash_profile.org > /dev/null 2>&1;
else
    rm -f $HOME/.bash_profile
fi
ln -s $HOME/github/dot.config/bash/.bash_profile $HOME/.bash_profile;

if [  ! -L $HOME/.bashrc ]; then
    mv -f $HOME/.bashrc $HOME/.bashrc.org > /dev/null 2>&1;
else
    rm -f $HOME/.bashrc
fi
ln -s $HOME/github/dot.config/bash/.bashrc $HOME/.bashrc;

touch $HOME/.bash_profile.priv;
touch $HOME/.bashrc.priv;

# Create symbolic links for bin scripts configuration.
if [ ! -d $HOME/bin ]; then
    mkdir $HOME/bin;
fi
if [  ! -L $HOME/bin/bin.utils ]; then
    mv -f $HOME/bin/bin.utils $HOME/bin/bin.utils.org > /dev/null 2>&1;
else
    rm -f $HOME/bin/bin.utils
fi
ln -s $HOME/github/dot.config/bin/bin.utils $HOME/bin/bin.utils

#################################################################################
# Platform specific aliases
#################################################################################
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    if [  ! -L $HOME/bin/bin.redhat ]; then
        mv -f $HOME/bin/bin.redhat $HOME/bin/bin.redhat.org > /dev/null 2>&1;
    else
        rm -f $HOME/bin/bin.redhat
    fi
    ln -s $HOME/github/dot.config/bin/bin.redhat $HOME/bin/bin.redhat
#elif [[ "$unamestr" == 'Darwin' ]]; then
fi

# Create symbolic links for emacs configuration.
if [  ! -L $HOME/.emacs ]; then
    mv -f $HOME/.emacs $HOME/.emacs.org > /dev/null 2>&1;
else
    rm -f $HOME/.emacs
fi
ln -s $HOME/github/dot.config/emacs.d/dot.emacs.el $HOME/.emacs;

if [  ! -L $HOME/.emacs.d ]; then
    mv -f $HOME/.emacs.d $HOME/.emacs.org > /dev/null 2>&1;
else
    rm -f $HOME/.emacs.d
fi
ln -s $HOME/github/dot.config/emacs.d/ $HOME/.emacs.d;

exit 0;
