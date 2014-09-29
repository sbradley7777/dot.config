#!/bin/sh
# Functions that are for bash.
# Some of thesee function are from here: https://github.com/mathiasbynens/dotfiles/blob/master/.functions

# Simple calculator
function calc() {
    local result=""
    result="$(printf "scale=10;$*\n" | bc --mathlib | tr -d '\\\n')"
    #                       └─ default (when `--mathlib` is used) is 20
    if [[ "$result" == *.* ]]; then
        # improve the output for decimal numbers
        printf "$result" |
        sed -e 's/^\./0./'        `# add "0" for cases like ".5"` \
            -e 's/^-\./-0./'      `# add "0" for cases like "-.5"`\
    -e 's/0*$//;s/\.$//'   # remove trailing zeros
    else
        printf "$result"
    fi
    printf "\n"
}

# Create a new directory and enter it
function mkd() {
    mkdir -p "$@" && cd "$@"
}

function trim_whitespaces() {
    local var=$@
    # remove leading whitespace characters
    var="${var#"${var%%[![:space:]]*}"}"
    # remove trailing whitespace characters
    var="${var%"${var##*[![:space:]]}"}"
    echo -n "$var"
}

# Create a .tar.gz archive, using `zopfli`, `pigz` or `gzip` for compression
#function targz() {
#    local tmpFile="${@%/}.tar"
#    tar -cvf "${tmpFile}" --exclude=".DS_Store" "${@}" || return 1
#    size=$(
#        stat -f"%z" "${tmpFile}" 2> /dev/null; # OS X `stat`
#        stat -c"%s" "${tmpFile}" 2> /dev/null # GNU `stat`
#    )
#    local cmd=""
#    if (( size < 52428800 )) && hash zopfli 2> /dev/null; then
#        # the .tar file is smaller than 50 MB and Zopfli is available; use it
#        cmd="zopfli"
#        else
#        if hash pigz 2> /dev/null; then
#            cmd="pigz"
#        else
#            cmd="gzip"
#        fi
#    fi

#    echo "Compressing .tar using \`${cmd}\`…"
#    "${cmd}" -v "${tmpFile}" || return 1
#    [ -f "${tmpFile}" ] && rm "${tmpFile}"
#    echo "${tmpFile}.gz created successfully."
#}

# Determine size of a file or total size of a directory
function fs() {
    if du -b /dev/null > /dev/null 2>&1; then
        local arg=-sbh
    else
        local arg=-sh
    fi
    if [[ -n "$@" ]]; then
        du $arg -- "$@"
    else
        du $arg .[^.]* *
    fi
}

# Use Git’s colored diff when available
hash git &>/dev/null
if [ $? -eq 0 ]; then
    function diff() {
        git diff --no-index --color-words "$@"
    }
fi

# Add bash completion for ssh: it tries to complete the host to which you
# want to connect from the list of the ones contained in ~/.ssh/known_hosts
# http://en.newinstance.it/2011/06/30/ssh-bash-completion/
# http://en.newinstance.it/2011/06/30/ssh-bash-completion/#comment-506408
_complete_hosts () {
    # http://surniaulula.com/2012/09/20/autocomplete-ssh-hostnames/
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    host_list=`{
	for c in /etc/ssh_config /etc/ssh/ssh_config ~/.ssh/config
	do [ -r $c ] && sed -n -e 's/^Host[[:space:]]//p' -e 's/^[[:space:]]*HostName[[:space:]]//p' $c
	done
	for k in /etc/ssh_known_hosts /etc/ssh/ssh_known_hosts ~/.ssh/known_hosts
	do [ -r $k ] && egrep -v '^[#\[]' $k|cut -f 1 -d ' '|sed -e 's/[,:].*//g'
	done
	sed -n -e 's/^[0-9][0-9\.]*//p' /etc/hosts; }|tr ' ' '\n'|grep -v '*'`
    COMPREPLY=( $(compgen -W "${host_list}" -- $cur))
    return 0
}
