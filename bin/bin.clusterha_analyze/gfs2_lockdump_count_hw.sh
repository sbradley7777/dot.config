#!/usr/bin/env bash
# This script counts the number of holders and waiters for a GFS2 lockdump or
# glocktop output.
#
# Usage: ./gfs2_lockdump_count_hw.sh <path to GFS2 lockdump or glocktop file>
#
#
# TODO:
# * Add getopts to give option to see count minimum for holder/waiter on
#   glock.
# * Add variable to hold name of GFS2 filesystem when analyzing glocktop
#   output. Append to end of print line of glock.

if [ -z $1 ]; then
   echo "ERROR: A path to the file that will be analyzed is required.";
   echo "Usage: $0 <path to GFS2 lockdump or glocktop file>";
   exit 1
fi
if [ ! -f "$1" ]; then
    echo "ERROR: The file does not exist: $1";
    echo "Usage: $0 <path to GFS2 lockdump or glocktop file>";
    exit 1
fi

# hw_count is the holder/waiter count.
hw_count=0;
current_glock="";
found_holder=1;
current_holder="";
while read line;do
    if [[ $line == G:* ]]; then
	if [ "$hw_count" -gt "1" ]; then
	    printf -v hc "%03d" $hw_count;
	    if [ "$found_holder" -eq "0" ]; then
		echo "$hc ---> $current_glock (Has Holder: $current_holder)";
	    else
		echo "$hc ---> $current_glock";
	    fi
	fi
	current_glock=$line;
	hw_count=0;
	found_holder=1;
	current_holder="";
    elif [[ $line == *H:* ]]; then
	((hw_count++));
	# f:AH|f:H|f:EH
	if [[ $line == *f:H* ]]; then
	    found_holder=0;
	    current_holder=$line;
	fi
    fi
done < $1
exit;
