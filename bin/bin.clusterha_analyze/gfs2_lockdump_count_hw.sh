#!/usr/bin/env bash
# This script counts the number of holders and waiters for a GFS2 lockdump or
# glocktop output.
#
# Usage: ./gfs2_lockdump_count_hw.sh -m <minimum number of waiters/holder> -p <path to GFS2 lockdump or glocktop file>
#
#
# TODO:
# * Add variable to hold name of GFS2 filesystem when analyzing glocktop
#   output. Append to end of print line of glock. Not capturing GFS2 filesystem
#   name in all cases. Need like an asterick at start of it.

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
# hw_count is the holder/waiter count.
hw_count=0;
current_glock="";
current_holder="";
current_gfs2_filesystem_name="";
while read line;do
    if [[ $line == G:* ]]; then
	if (( $hw_count >= $minimum_hw )); then
	    printf -v hc "%03d" $hw_count;
	    if [ -n "$current_holder" ]; then
		echo "$hc ---> $current_glock [$current_gfs2_filesystem_name] (Has Holder: $current_holder)";
	    else
		echo "$hc ---> $current_glock [$current_gfs2_filesystem_name]";
	    fi
	fi
	hw_count=0;
	current_glock=$line;
	current_holder="";
    elif [[ $line == *H:* ]]; then
	((hw_count++));
	# f:AH|f:H|f:EH
	if [[ $line == *f:H* ]]; then
	    current_holder=$line;
	fi
    elif [[ $line == *I:* ]]; then
	continue;
    elif [[ $line == *R:* ]]; then
	continue;
    elif [[ $line == *D:* ]]; then
	continue;
    elif [[ "$line" =~ [^a-zA-Z0-9] ]]; then
	current_gfs2_filesystem_name=`echo $line | cut -d " " -f1`;
    fi
done < $path_to_file;
exit;
