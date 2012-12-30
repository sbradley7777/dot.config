#################################################################################
# Generic aliases
#################################################################################
source $HOME/github/dot.config/aliases/aliases.all

#################################################################################
# Platform specific aliases
#################################################################################
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    source $HOME/github/dot.config/aliases/aliases.redhat
elif [[ "$unamestr" == 'Darwin' ]]; then
    source $HOME/github/dot.config/aliases/aliases.osx
fi

#################################################################################
# Source in various other aliases
#################################################################################
# For private aliases that will only reside on this machine
if [ -e $HOME/.aliases.priv ]; then
    source $HOME/.aliases.priv
fi

# source $HOME/github/dot.config/aliases/aliases.devel



