#!/bin/sh
# If the files are not symbolic links then they will be backed up. If the files
# are symbolic links then they will be removed and readded so that paths are
# correct.

# The copying of cluster files requires sudo access.

function create_symbolic_link() {
    # Create symbolic links for bash configuration. $1 src $2 path to link.
    if [ -e $1 ]; then
        if [  ! -L $2 ]; then
            mv -f $2 $2.org > /dev/null 2>&1;
        else
            rm -f $2
        fi
        ln -s $1 $2
    #else
    #    echo "$1";
    fi
}

function gcopy() {
    # Copies the files to a directory. $1 is the source $2 is the destination.
    if [ -e $1 ]; then
        if [  ! -L $2 ]; then

            basename=${2##*/};
            basepath=${2%/*};
            case $basename in
                .*) mv -f $2 $2.org > /dev/null 2>&1;;
                *)  mv -f $2 $basepath/.$basename.org  > /dev/null 2>&1;;
            esac
        else
            rm -f $2
        fi
        printf "INFO: Copying the files $1 to $2.\n"
        cp -rf $1 $2
        # Remove all the README.md files that were copied over.
        find $2 -name README.md -exec rm -f {}  \;
    fi
}


# Create bash links.
gcopy $HOME/github/dot.config/bash/.bash_profile $HOME/.bash_profile
gcopy $HOME/github/dot.config/bash/.bashrc $HOME/.bashrc;
gcopy $HOME/github/dot.config/bash/.aliases.all $HOME/.aliases.all;
gcopy $HOME/github/dot.config/bash/.aliases.devel $HOME/.aliases.devel;
touch $HOME/.bash_profile.priv;
touch $HOME/.bashrc.priv;
gcopy $HOME/github/dot.config/bash/.functions.sh $HOME/.functions.sh;

# Create symbolic links for emacs configuration.
gcopy $HOME/github/dot.config/emacs.d/dot.emacs.el $HOME/.emacs;
gcopy $HOME/github/dot.config/emacs.d/ $HOME/.emacs.d;

# Create symbolic links for git configs
gcopy $HOME/github/dot.config/conf/.gitconfig $HOME/.gitconfig
gcopy $HOME/github/dot.config/conf/.gitignore $HOME/.gitignore

# Copy bin scripts.
if [ ! -d $HOME/bin ]; then
    mkdir $HOME/bin;
fi
gcopy $HOME/github/dot.config/bin/bin.utils $HOME/bin/bin.utils

# Platform specific links to add for bin scripts.
unamestr=`uname`
if [[ "$unamestr" == "Linux" ]]; then
    gcopy $HOME/github/dot.config/bash/.aliases.devel $HOME/.aliases.devel;
    gcopy $HOME/github/dot.config/bash/.aliases.redhat $HOME/.aliases.redhat;
    gcopy $HOME/github/dot.config/bash/.aliases.sx $HOME/.aliases.sx;
    gcopy $HOME/github/dot.config/bin/bin.redhat $HOME/bin/bin.redhat
    if [[ `rpm --qf %{NAME} -q cman` == "cman" ]]; then
        gcopy $HOME/github/dot.config/bin/bin.clusterha $HOME/bin/bin.clusterha
        gcopy $HOME/github/dot.config/bin/bin.clusterha_probe $HOME/bin/bin.clusterha_probe
        # Instead of creating symlink, copy and backup an existing file if it
        # exists which will require root access.
        if [ $(id -u) -eq 0 ];then
            if [ ! -d /etc/cluster/scripts ]; then
                # Requires to be ran as root.
                mkdir -p /etc/cluster/scripts;
                if [ -d /etc/cluster/scripts ]; then
                    cp --backup $HOME/github/dot.config/etc/cluster/scripts/test_script.sh /etc/cluster/scripts/test_script.sh;
                fi
            fi
        fi
    fi
elif [[ "$unamestr" == 'Darwin' ]]; then
    gcopy $HOME/github/dot.config/bash/.aliases.osx $HOME/.aliases.osx;
fi

printf "INFO: Installation complete. Relogin for changes to take affect.\n";
exit 0;
