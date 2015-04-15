#!/usr/bin/env bash
# Author: sbradley@redhat.com
# Description: This script counts the number of holders and waiters for a GFS2
#              lockdump or glocktop output.
# Version: 1.3
#
# Usage: ./gfs2_lockdump_count_hw.sh -m <minimum number of waiters/holder> -p <path to GFS2 lockdump or glocktop file>
#
# TODO:
# * Move the summary to the main loop instead of doing a grep 4 times, better to
#   do once in loop.
# * See if finding the filesystem name/year could be done better.

bname=$(basename $0);
usage()
{
cat <<EOF
usage: $bname -m <minimum number of waiters/holder> -p <path to GFS2 lockdump or glocktop file>

This script counts the number of holders and waiters for a GFS2 lockdump or glocktop output.

OPTIONS:
   -h      Show this message
   -m      Minimum number of waiters/holder that a glock has.
   -p      Path to the file that will be analyzed.

EXAMPLE:
$ $bname -m 5 -p ~/myGFS2.glocks

EOF
}

path_to_file="";
minimum_hw=2;
while getopts ":hm:p:" opt; do
    case $opt in
	h)
	    usage;
	    exit;
	    ;;
	m)
	    minimum_hw=$OPTARG;
	    ;;
	p)
	    path_to_file=$OPTARG;
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
   echo "ERROR: A path to the file that will be analyzed is required with -p option.";
   usage;
   exit 1
fi
if [ ! -n $path_to_file ]; then
   echo "ERROR: A path to the file that will be analyzed is required with the -p option.";
   usage;
   exit 1
fi
if [ ! -f $path_to_file ]; then
    echo "ERROR: The file does not exist: $1";
    usage;
    exit 1
fi

# Print some useful stat information before doing the count on holder/waiters on
# each glock.
echo -e "Processing the file: $path_to_file";
printf "  inodes:    "; egrep -ri 'n:2' $path_to_file | wc -l;
printf "  rgrp:      "; egrep -ri 'n:3' $path_to_file | wc -l;
printf "  Waiters:   "; egrep -ri 'f:w|f:aw|f:cw|f:ew|f:tw' $path_to_file | wc -l;
printf "  Holders:   "; egrep -ri 'f:ah|f:h' $path_to_file | wc -l;
echo -e  "----------------------------------------------------------------------------------------";

# hw_count is the holder/waiter count.
hw_count=0;
cglock="";
cholder="";
cgfs2_filesystem_name="";
ctimestamp="";

shopt -s extglob

while read line;do
    if [ ! -n "${line##+([[:space:]])}" ]; then
	continue;
    elif [[ $line == G:* ]]; then
	if (( $hw_count >= $minimum_hw )); then
	    printf -v hc "%03d" $hw_count;
	    if [ -n $cgfs2_filesystem_name ]; then
		echo "$hc ---> $cglock [$cgfs2_filesystem_name] [$ctimestamp]";
	    else
		echo "$hc ---> $cglock";
	    fi

	    if [ -n "$cholder" ]; then
		echo "             $cholder (HOLDER)";
	    fi
	fi
	hw_count=0;
	cglock=$line;
	cholder="";
    elif [[ $line == *H:* ]]; then
	((hw_count++));
	# f:AH|f:H|f:EH
	if [[ $line == *f:H* ]]; then
	    cholder=$line;
	fi
    elif [[ $line == *I:* ]]; then
	continue;
    elif [[ $line == *R:* ]]; then
	continue;
    elif [[ $line == *D:* ]]; then
	continue;
    # There has to be better way to check for the fsname/date separators.
    elif [[ ${line:0:1} =~ ^[a-zA-Z0-9] ]]; then
	cgfs2_filesystem_name="`echo $line | cut -d \" \" -f1`";
	ctimestamp_tmp="`echo $line | awk '{print $3,$4,$6,$5;}'`";
	ctimestamp_tmp=`date -d "$ctimestamp_tmp" +%Y-%m-%d_%H:%M:%S 2>/dev/null`;
        # If string starts with year then change current timestamp.
	if [[ $ctimestamp_tmp =~ ^20[0-9][0-9]-* ]];then
	    ctimestamp=$ctimestamp_tmp;
	fi
    fi
done < $path_to_file;
exit;

