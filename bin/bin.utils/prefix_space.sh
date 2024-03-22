#!/bin/bash
######################################################################
# prefix-space.sh                                                    #
#                                                                    #
# Author: Shane Bradley(sbradley@redhat.com)                         #
# Creation Date: 3-15-2024                                           #
# Version: 2.0                                                       #
#                                                                    #
# usage:                                                             #
# $ prefix-space.sh -p <path to file> -w <whitespace count>          #
######################################################################

# A list of strings to ignore.
GREP_IGNORES=(    'sshd' \
                  'goferd' \
                  'sudo' \
		  'org.freedesktop' \
                  'systemd-logind' \
                  'rate-limiting' \
		  "http://www.rsyslog.com" \
		  "journal\: Suppressed" \
		  "of user root" \
		  "Audit daemon rotating log files" \
		  "User Slice of" \
                  "su\:" \
                  "su\[" \
                  "is marked world-inaccessible" \
                  "is marked executable" \
		  'Started Session' );


usage() {
    bname=$(basename $0);
    echo -e "usage: $bname -s <path to file> -w <prefix whitespace count>";
    echo -e "This script counts the number of holders and waiters for a GFS2 lockdump or glocktop output.\n";
    echo "OPTIONS:";
    echo "   -h      show this message";
    echo "   -p      path to the file that will be analyzed";
    echo "   -w      prefix whitespace count (default: 2)";
    echo -e "\nEXAMPLE:";
    echo "$ $bname -s ~/somefile.txt -w  5";
}

path_to_file="";
prefix_whitespace_count=2;
while getopts ":hp:w:" opt; do
    case $opt in
    h)
        usage;
        exit;
        ;;
    p)
        path_to_file=$OPTARG;
        ;;
    w)
        prefix_whitespace_count=$OPTARG;
        ;;
    \?)
        echo "Invalid option: -$OPTARG" >&2
        exit 1
        ;;
    :)
        echo "Option -$OPTARG requires an argument." >&2
        exit 1
        ;;
    esac
done

if [ -z $path_to_file ]; then
    echo -e "ERROR: A path to the file that will be read is required with -p option.\n";
    usage;
    exit 1
fi
if [ ! -n $path_to_file ]; then
    echo -e "ERROR: A path to the file that will be read is required with the -p option.\n";
    usage;
    exit 1
fi
if [ ! -f $path_to_file ]; then
    echo -e "ERROR: The file does not exist: $1\n";
    usage;
    exit 1
fi

# Create a string that contains the number of spaces that will be prefixed for each line.
prefix=$(printf "%*s" $prefix_whitespace_count)
# Enable bash debugging by uncommenting this line.
#set -x

# Build a list of grep -e <string> options to exclude from the results.
grep_ignore_regexs="";
for i in ${!GREP_IGNORES[@]}; do
    # grep_ignore_regexs+="-e '${GREP_IGNORES[i]}' ";
    grep_ignore_regexs+="-e '${GREP_IGNORES[i]}' ";
done;
# Command to add spacing, then strip lines of strings that should be ignored.
# Had to use "eval" as if ran direct it would add lots of escape quotes.
eval "grep -ai -v $grep_ignore_regexs $path_to_file" | awk -v prefix="$prefix" '{print prefix $0}';
exit;
