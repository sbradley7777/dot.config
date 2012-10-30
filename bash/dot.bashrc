#################################################################################
# Platform specific aliases
#################################################################################
# Find out platform of host
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    source $HOME/github/dot.config/aliases/aliases.redhat
elif [[ "$unamestr" == 'Darwin' ]]; then
    source $HOME/github/dot.config/aliases/aliases.osx
fi

#################################################################################
# Generic aliases
#################################################################################
alias df="df -h"
alias today="date '+%F_%s'"

#################################################################################
# Source in various other aliases
#################################################################################
source $HOME/github/dot.config/aliases/aliases.devel

# For private aliases that will only reside on this machine
if [ -e $HOME/.aliases.priv ]; then
    source $HOME/.aliases.priv
fi


