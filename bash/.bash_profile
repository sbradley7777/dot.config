#!/bin/sh
# .bash_profile

##############################################################################
# Environment Variables
##############################################################################
export EDITOR=emacs
export TODAY=`date '+%F_%s'`
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;31'
export HISTFILESIZE=10000
##############################################################################
# Change PS prompt if root, else set as normal user
##############################################################################
if [ $(id -u) -eq 0 ];then
    export PS1='\[\033[01;36m\]\u@\h\[\033[00m\]:\[\033[01;33m\]\w\[\033[00m\]\$ '
else
    export PS1='\[\033[01;31m\]\u@\h\[\033[00m\]:\[\033[01;33m\]\W\[\033[00m\]\$ '
fi

##############################################################################
# Set color variables
##############################################################################
# Enable color output
export CLICOLOR=1
# OSX uses LSCOLORS and linux uses LS_COLORS
export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'
# Colors
# a     black
# b     red
# c     green
# d     brown
# e     blue
# f     magenta
# g     cyan
# h     light grey
# A     bold black, usually shows up as dark grey
# B     bold red
# C     bold green
# D     bold brown, usually shows up as yellow
# E     bold blue
# F     bold magenta
# G     bold cyan
# H     bold light grey; looks like bright white
# x     default foreground or background

# The order of the attributes are as follows:
# 1.   directory
# 2.   symbolic link
# 3.   socket
# 4.   pipe
# 5.   executable
# 6.   block special
# 7.   character special
# 8.   executable with setuid bit set
# 9.   executable with setgid bit set
# 10.  directory writable to others, with sticky bit
# 11.  directory writable to others, without sticky bit
#                1 2 3 4 5 6 7 8 9 a b
export LSCOLORS="exgxcxdxcxegedabagacad"

##############################################################################
# Non-Standard Environment Variables
##############################################################################
# Path to location of all the alias files.
export GITHUB_DOT_CONFIGS=$HOME/github/dot.config
export GITHUB_ALIASES=$GITHUB_DOT_CONFIGS/aliases

##############################################################################
# PATH variable
##############################################################################
if [ -d $HOME/bin ]; then
    # export all directories under ~/bin that are symbolic links. There is no
    # error checking for if link is file or directory.
    export PATH=$PATH:$HOME/bin
    for dir in $HOME/bin/*; do
        if [ -L $dir ]; then
            PATH=$PATH:$dir
        fi
    done;
fi

##############################################################################
# Build own hosts file
##############################################################################
# Collect host names from SSH known hosts file for tab autocomplete. Use our own
# file instead of /etc/hosts
if [ -f $HOME/.ssh/known_hosts ];then
    sed  's/\(.*\),.*/\1/; s/ .*//' ${HOME}/.ssh/known_hosts  | sort > ${HOME}/.hosts
    if [ -f ${HOME}/.hosts ]; then
        export HOSTFILE=${HOME}/.hosts
    fi
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
