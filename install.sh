#!/bin/sh
# If the files are not symbolic links then they will be backed up. If the files
# are symbolic links then they will be removed and readded so that paths are
# correct.

function create_symbolic_link() {
    # Create symbolic links for bash configuration. $1 src $2 path to link.
    if [ -e $1 ]; then
        if [  ! -L $2 ]; then
            mv -f $2 $2.org > /dev/null 2>&1;
        else
            rm -f $2
        fi
        ln -s $1 $2
    fi
}

# Create bash links.
create_symbolic_link $HOME/github/dot.config/bash/.bash_profile $HOME/.bash_profile
create_symbolic_link $HOME/github/dot.config/bash/.bashrc $HOME/.bashrc;
touch $HOME/.bash_profile.priv;
touch $HOME/.bashrc.priv;

# Create symbolic links for emacs configuration.
create_symbolic_link $HOME/github/dot.config/emacs.d/dot.emacs.el $HOME/.emacs;
create_symbolic_link $HOME/github/dot.config/emacs.d/ $HOME/.emacs.d;

# Create symbolic links for bin scripts.
if [ ! -d $HOME/bin ]; then
    mkdir $HOME/bin;
fi
create_symbolic_link $HOME/github/dot.config/bin/bin.utils $HOME/bin/bin.utils

# Platform specific links to add for bin scripts.
unamestr=`uname`
if [[ "$unamestr" == "Linux" ]]; then
    create_symbolic_link $HOME/github/dot.config/bin/bin.redhat $HOME/bin/bin.redhat
    if [[ `rpm --qf %{NAME} -q cman` == "cman" ]]; then
        create_symbolic_link $HOME/github/dot.config/bin/bin.clusterha $HOME/bin/bin.clusterha
        if [ ! -d /etc/cluster/scripts ]; then
            mkdir -p /etc/cluster/scripts;
        fi
        # Instead of creating symlink, copy and backup an existing file if it
        # exists.
        cp --backup $HOME/github/dot.config/etc/cluster/scripts/test_script.sh /etc/cluster/scripts/test_script.sh
    fi
fi

echo "Installation successful. Relogin for changes to take affect.";
exit 0;
