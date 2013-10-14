#!/bin/sh
# .bashrc

#################################################################################
# Source global definitions
#################################################################################
# Comment out cause there is nothing that is pulled in that I need.
#if [ -f /etc/bashrc ]; then
#   source  /etc/bashrc
#fi
# Path to location of all the alias files.
export GITHUB_DOT_CONFIGS=$HOME/github/dot.config;
export GITHUB_ALIASES=$GITHUB_DOT_CONFIGS/aliases;

#################################################################################
# Generic aliases
#################################################################################
if [ -f $GITHUB_ALIASES/aliases.all ]; then
    source $GITHUB_ALIASES/aliases.all
fi

#################################################################################
# Platform specific aliases
#################################################################################
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    if [ -f $GITHUB_ALIASES/aliases.redhat ]; then
        source $GITHUB_ALIASES/aliases.redhat
    fi
elif [[ "$unamestr" == 'Darwin' ]]; then
    if [ -f $GITHUB_ALIASES/aliases.osx ]; then
        source $GITHUB_ALIASES/aliases.osx
    fi
fi

#################################################################################
# Source in devel aliases
#################################################################################
if [ -f $GITHUB_ALIASES/aliases.devel ]; then
    source $GITHUB_ALIASES/aliases.devel
fi
#################################################################################
# Source in various other aliases
#################################################################################
# For private aliases that will only reside on this machine
if [ -f $HOME/.bashrc.priv ]; then
    source $HOME/.bashrc.priv
fi




