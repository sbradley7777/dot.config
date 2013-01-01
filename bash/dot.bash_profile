#!/bin/sh
# .bash_profile

##############################################################################
# Environment Variables
##############################################################################
export EDITOR=emacs
export TODAY=`date '+%F_%s'`
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;31'
export PATH=$PATH:$HOME/bin:$HOME/Desktop/bin

##############################################################################
# Change PS prompt if root, else set as normal user
##############################################################################
if [ $(id -u) -eq 0 ];then
    export PS1='\[\033[01;36m\]\u@\h\[\033[00m\]:\[\033[01;33m\]\w\[\033[00m\]\# '
else
    export PS1='\[\033[01;31m\]\u@\h\[\033[00m\]:\[\033[01;33m\]\W\[\033[00m\]\$ '
fi

##############################################################################
# Non-Standard Environment Variables
##############################################################################
# Path to location of all the alias files.
export GITHUB_ALIASES=$HOME/github/dot.config/aliases

##############################################################################
# Build own hosts file
##############################################################################
# Use our own file instead of /etc/hosts
export HOSTFILE="${HOME}/.hosts"

# Collect host names from SSH known hosts file for tab autocomplete.
if [ -f $HOME/.ssh/known_hosts ];then
    sed  's/\(.*\),.*/\1/; s/ .*//' ${HOME}/.ssh/known_hosts  | sort > ${HOME}/.hosts
fi

##############################################################################
# Source in the .bash_profile.priv for private configuration
##############################################################################
if [ -f $HOME/.bash_profile.priv ];then
    source $HOME/.bash_profile.priv
fi
##############################################################################
# Source in the .bashrc for alias and such
##############################################################################
if [ -f $HOME/.bashrc ]; then
    source $HOME/.bashrc
fi
