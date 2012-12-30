##############################################################################
# Environment Variables
##############################################################################
export EDITOR=emacs
export TODAY=`date '+%F_%s'`
export PS1='\[\033[01;31m\]\u@\h\[\033[00m\]:\[\033[01;33m\]\W\[\033[00m\]\$ '
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='0;31'

##############################################################################
# Build own hosts file
##############################################################################
# Use our own file instead of /etc/hosts
export HOSTFILE="${HOME}/.hosts"
# Collect host names from SSH known hosts file
sed  's/\(.*\),.*/\1/; s/ .*//' ${HOME}/.ssh/known_hosts  | sort > ${HOME}/.hosts

##############################################################################
# source .bash_profile.priv for private configuration
##############################################################################
source ~/.bash_profile.priv

##############################################################################
# source .bashrc for alias and such
##############################################################################
source ~/.bashrc

