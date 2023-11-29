#!/bin/bash

#prefix_whitespace_count=2
#prefix=$(printf "%*s" $prefix_whitespace_count)
#cat $1 | awk -v prefix="$prefix" '{print prefix $0}';



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

# Make sure a path to lockdumps was given.
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

prefix=$(printf "%*s" $prefix_whitespace_count)
#cat $path_to_file | awk -v prefix="$prefix" '{print prefix $0}'
cat $path_to_file | awk -v prefix="$prefix" '{print prefix $0}' | grep -v "journal\: Suppressed" | grep -v "of user root" | grep -v "Audit daemon rotating log files" | grep -v "User Slice of" | grep -v "systemd-logind" | grep -v "rate-limiting" | grep -v "Started Session" | grep -v "http://www.rsyslog.com";
exit;
