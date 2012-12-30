#################################################################################
# Generic aliases
#################################################################################
source $GITHUB_ALIASES/aliases.all

#################################################################################
# Platform specific aliases
#################################################################################
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    source $GITHUB_ALIASES/aliases.redhat
elif [[ "$unamestr" == 'Darwin' ]]; then
    source $GITHUB_ALIASES/aliases.osx
fi

#################################################################################
# Source in devel aliases
#################################################################################
source $GITHUB_ALIASES/aliases.devel

#################################################################################
# Source in various other aliases
#################################################################################
# For private aliases that will only reside on this machine
if [ -e $HOME/.aliases.priv ]; then
    source $HOME/.aliases.priv
fi




