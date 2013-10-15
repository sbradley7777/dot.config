#!/bin/sh
# .bashrc

#################################################################################
# Source global definitions
#################################################################################
# Comment out cause there is nothing that is pulled in that I need.
#if [ -f /etc/bashrc ]; then
#   source  /etc/bashrc
#fi


#################################################################################
# Source in all aliases.
#################################################################################
for i in $( ls $HOME/.aliases.*); do
    if [[ ! "${i##*.}" == org ]]
    then
        source $i
    fi
done

#################################################################################
# Platform specific aliases
#################################################################################
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    if [ -f $HOME/.aliases.redhat ]; then
        source $HOME/.aliases.redhat
        source $HOME/.aliases.sx
    fi
elif [[ "$unamestr" == 'Darwin' ]]; then
    if [ -f $HOME/.aliases.osx ]; then
        source $HOME/.aliases.osx
    fi
fi

#################################################################################
# Source in various other aliases
#################################################################################
# For private aliases that will only reside on this machine
if [ -f $HOME/.bashrc.priv ]; then
    source $HOME/.bashrc.priv
fi




