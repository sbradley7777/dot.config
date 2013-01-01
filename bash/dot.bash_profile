#!/bin/sh
# .bash_profile

##############################################################################
# Environment Variables
##############################################################################
export EDITOR=emacs
export TODAY=`date '+%F_%s'`
export PS1='\[\033[01;31m\]\u@\h\[\033[00m\]:\[\033[01;33m\]\W\[\033[00m\]\$ '
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;31'

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
sed  's/\(.*\),.*/\1/; s/ .*//' ${HOME}/.ssh/known_hosts  | sort > ${HOME}/.hosts

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
