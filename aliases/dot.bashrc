# .bashrc
# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

##############################################################################
# bash command aliases
##############################################################################
alias ll='ls -l --color=auto'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias emacs='emacs -nw'
alias today='date +"%F_%s"'

##############################################################################
# Git Repos
##############################################################################
alias gitcloneemacs='mkdir -p ~/github/; cd ~/github/; git clone https://github.com/sbradley7777/emacs.d.git; cd ~/; ln -s ~/github/emacs.d/ ~/.emacs.d; ln -s ~/github/emacs.d/dot.emacs ~/.emacs'
alias gitclonesx='mkdir -p ~/git/; cd ~/git; mv ~/git/sx ~/git/sx.bk; git clone ssh://git.fedorahosted.org/git/sx.git'

