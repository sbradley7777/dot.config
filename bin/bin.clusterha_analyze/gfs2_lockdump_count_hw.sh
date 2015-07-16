#!/usr/bin/env bash
# Author: sbradley@redhat.com
# Description: This script counts the number of holders and waiters for a GFS2
#              lockdump or glocktop output. In addition, any glocks with a high
#              time to demote a lock will be printed.
# Version: 1.7
#
# Usage: ./gfs2_lockdump_count_hw.sh -s -m <minimum number of waiters/holder> -p <path to GFS2 lockdump or glocktop file>
#
# TODO:
# * See if finding the filesystem name/year could be done better.
# * Options that will show all waiters or entire glock trace.
# * Option to search for specific glocks to see how there holder/waiter
#   count changes over time. Could map all of them but that really
#   require leverging rewrite in python.

bname=$(basename $0);
usage()
{
cat <<EOF
usage: $bname -m <minimum number of waiters/holder> -p <path to GFS2 lockdump or glocktop file>

This script counts the number of holders and waiters for a GFS2 lockdump or glocktop output.

OPTIONS:
   -h      show this message
   -m      minimum number of waiters/holder that a glock has
   -p      path to the file that will be analyzed
   -s      enable summary of glock types
   -w      print the waiters

EXAMPLE:
$ $bname -s -m 5 -p ~/myGFS2.glocks

EOF
}

path_to_file="";
minimum_hw=2;
enable_glock_summary=1;
show_waiters=1;

while getopts ":hm:p:sw" opt; do
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
	s)
	    enable_glock_summary=0;
	    ;;
	w)
	    show_waiters=0;
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

# chw_count is the holder/waiter count for current glock.
chw_count=0;
cglock="";
cholder="";
cwaiters="";
cgfs2_filesystem_name="";
ctimestamp="";

# Summary stats
inode_count=0;
rgrp_count=0;
waiter_count=0;
holder_count=0;

echo "Searching the file for glocks with holder+waiter count >= $minimum_hw or high demote of glock time: `basename $path_to_file`.";
shopt -s extglob
while read line;do
    if [ ! -n "${line##+([[:space:]])}" ]; then
	continue;
    elif [[ $line == G:* ]]; then
	if [[ $line == *n:2* ]]; then
	    ((inode_count++));
	elif [[ $line == *n:3* ]]; then
	    ((rgrp_count++))
	fi
	demote_time=$(echo $cglock | awk '{print $6}' | cut -d "/" -f2);
	demote_time_warning="";
	if [[ "$demote_time" -gt "0" ]]; then
	    demote_time_warning="**The demote time is greater than 0.**";
	fi
	# If another G: line starts then print information about the previous
	# one if it meets minimum requirements.
	if (( $chw_count >= $minimum_hw )) || [[ "$demote_time" -gt "0" ]]; then
	    printf -v hc "%03d" $chw_count;
	    if [ -z $cgfs2_filesystem_name ]; then
		echo "$hc ---> $cglock $demote_time_warning";
	    else
		echo "$hc ---> $cglock [$cgfs2_filesystem_name] [$ctimestamp] $demote_time_warning";
	    fi
	    if [ -n "$cholder" ]; then
		echo "           $cholder (HOLDER)";
	    fi
	    if (( $show_waiters == 0 )); then
		(
                    # Only change in subshell the IFS var and not globally.
		    IFS=$'\n'
		    for waiter_line in $(echo -e $cwaiters); do
			printf '          %s\n' "$waiter_line"
		    done
		)
	    fi
	fi
	# Set the vars for the current glock.
	chw_count=0;
	cglock=$line;
	cholder="";
	cwaiters="";
    elif [[ $line == *H:* ]]; then
	((chw_count++));
	# f:AH|f:H|f:EH
	if [[ $line == *f:*H* ]]; then
	    cholder=$line;
	    ((holder_count++));
	elif [[ $line == *f:*W* ]]; then
	    ((waiter_count++));
	    #cwaiters+=$'\n'$line;
	    cwaiters+="\n$line";
	fi
    elif [[ $line == *I:* ]]; then
	continue;
    elif [[ $line == *R:* ]]; then
	continue;
    elif [[ $line == *D:* ]]; then
	continue;
    # There has to be better way to check for the fsname/date separators.
    elif [[ ${line:0:1} =~ ^[a-zA-Z0-9] ]]; then
	# If another GFS2 filesystem is encountered then then print information
	# about the previous one if it meets minimum requirements.
	if (( $chw_count >= $minimum_hw )); then
	    printf -v hc "%03d" $chw_count;
	    if [ -n $cgfs2_filesystem_name ]; then
		echo "$hc ---> $cglock [$cgfs2_filesystem_name] [$ctimestamp]";
	    else
		echo "$hc ---> $cglock";
	    fi
	    if [ -n "$cholder" ]; then
		echo "           $cholder (HOLDER)";
	    fi
	    if (( $show_waiters == 0 )); then
		(
                    # Only change in subshell the IFS var and not globally.
		    IFS=$'\n'
		    for waiter_line in $(echo -e $cwaiters); do
			printf '          %s\n' "$waiter_line"
		    done
		)
	    fi
	fi
	# Reset the current glocks vars.
	chw_count=0;
	# Set to empty string, cause this is not a glock line.
	cglock="";
	cholder="";
	cwaiters="";
	cgfs2_filesystem_name="`echo $line | cut -d \" \" -f1`";
	ctimestamp_tmp="`echo $line | awk '{print $3,$4,$6,$5;}'`";
	ctimestamp_tmp=`date -d "$ctimestamp_tmp" +%Y-%m-%d_%H:%M:%S 2>/dev/null`;
        # If string starts with year then change current timestamp.
	if [[ $ctimestamp_tmp =~ ^20[0-9][0-9]-* ]];then
	    ctimestamp=$ctimestamp_tmp;
	fi
    fi
done < $path_to_file;

if (( $enable_glock_summary == 0 )); then
    # Print some useful stat information before doing the count on
    # holder/waiters on each glock.
    echo -e  "\n----------------------------------------------------------------------------------------";
    echo -e "Summary Stats";
    echo -e  "----------------------------------------------------------------------------------------";
    echo "  inodes:    $inode_count";
    echo "  rgrp:      $rgrp_count";
    echo "  Waiters:   $waiter_count";
    echo "  Holders:   $holder_count";
fi 
exit;

