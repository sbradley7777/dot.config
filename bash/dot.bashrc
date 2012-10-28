#################################################################################
# Platform specific aliases
#################################################################################
# Find out platform of host
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    source $HOME/github/dot.config/aliases/dot.bashrc.redhat
elif [[ "$unamestr" == 'Darwin' ]]; then
    source $HOME/github/dot.config/aliases/dot.bashrc.osx
fi

#################################################################################
# Generic aliases
#################################################################################
alias df="df -h"
alias diff='diff -pruN'
alias today="date '+%F_%s'"

#################################################################################
# Source in various other aliases
#################################################################################
source $HOME/github/dot.config/aliases/dot.bashrc.devel


