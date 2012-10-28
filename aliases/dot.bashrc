# .bashrc
# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    source ~/github/dot.config/aliases/dot.bashrc.redhat
elif [[ "$unamestr" == 'Darwin' ]]; then
    source ~/github/dot.config/aliases/dot.bashrc.osx
fi

##############################################################################
# bash command aliases
##############################################################################
alias grep='grep --color=auto'
alias today='date +"%F_%s"'

##############################################################################
# Git Repos
##############################################################################
alias gitcloneemacs='mkdir -p ~/github/; cd ~/github/; git clone https://github.com/sbradley7777/emacs.d.git; cd ~/; ln -s ~/github/emacs.d/ ~/.emacs.d; ln -s ~/github/emacs.d/dot.emacs ~/.emacs'
alias gitclonesx='mkdir -p ~/git/; cd ~/git; mv ~/git/sx ~/git/sx.bk; git clone ssh://git.fedorahosted.org/git/sx.git'

