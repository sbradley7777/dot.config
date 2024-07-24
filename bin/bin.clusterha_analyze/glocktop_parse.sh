#!/usr/bin/env bash
# Author: sbradley@redhat.com
# Description: A script that will parse glocktop files and output formatted data.
#
# Version: 1.0

usage()
{
    script_name=$(basename $0);
    cat <<EOF
usage: $script_name -p <path to the glocktop file> -n <filesystem name> -f -l -s

This script will parse glocktop files and output formatted data.

OPTIONS:
   -h      show this message
   -p      path to the glocktop file that will be analyzed
   -n      name of the gfs2 filesystem
   -l      list the gfs2 filesystems in the glocktop file
   -f      show count of functions that processes are in
   -s      summary of glocktop file

EXAMPLES:
  List the gfs2 filesystems in the glocktop file.
    $ $script_name -p <path to the glocktop file> -l

  Parse the glocktop file for particular filesystem name.
    $ $script_name -p <path to the glocktop file> -n <name of the gfs2 filesystem>

  List the count for each function that processes are in.
    $ $script_name -p <path to the glocktop file> -f

  Show summary of glocktop file.
    $ $script_name -p <path to the glocktop file> -s

EOF
}

# Option variables.
path_to_file="";
filesystem_name="";
list_filesystems=1;
show_function_count=1;
show_summary=1;

# Parse the options.
while getopts "hp:n:lfs" opt; do
    case $opt in
	h)
	    usage;
	    exit;
	    ;;
	p)
	    path_to_file=$OPTARG;
	    ;;
	n)
	    filesystem_name=$OPTARG;
	    ;;
	l)
	    list_filesystems=0;
	    ;;
	f)
	    show_function_count=0;
	    ;;
	s)
	    show_summary=0;
	    ;;
    esac
done

show_glocks() {
    sed s/"^@"/"\n@"/g $1 | awk "/@ $2/,/^$/" | grep -ie "@" -ie "G\:" -ie "H\: "| grep -v -ie "Held SH" -ie "S G Waiting" -ie "S P Waiting" -ie ended;
}

list_filesystems() {
   grep "@" $1 | awk '{print $2}' | sort | uniq;
}

show_function_count() {
    fs_names=( $2 )
    if [ ${#fs_names[@]} -eq 0 ]; then
	mapfile -t fs_names < <( list_filesystems $1 )
    fi
    for fs_name in "${fs_names[@]}"; do
	echo $fs_name;
	sed s/"^@"/"\n@"/g $1 | awk "/@ $fs_name/,/^$/" | grep "H:" | grep -v -ie "Held SH" -ie "S G Waiting" -ie "S P Waiting" | cut -d "]" -f 2 | cut -d "[" -f 1 | sort | uniq -c | sort -rnk1;
    done
}

summarize_filesystem() {
    echo "Future feature";
    echo $1;
    echo $2;
}

show_summary() {
    fs_names=( $2 )
    if [ ${#fs_names[@]} -eq 0 ]; then
        mapfile -t fs_names < <( list_filesystems $1 )
    fi
    for fs_name in "${fs_names[@]}"; do
	summarize_filesystem $1 $fs_name;
    done
}


# ####################################################################
# Validate Options
# ####################################################################
# Verify a file has been passed.
if [ -z $path_to_file ] || [ ! -n $path_to_file ]; then
   echo "ERROR: A path to the glocktop file (-p) is required. The script will exit.";
   exit 1;
elif [ ! -f $path_to_file ]; then
    echo "ERROR: The glocktop file does not exist: $path_to_file.";
    exit 1;
fi

# Verify that a filesystem name was given and it exists.
if [ -z $filesystem_name ] || [ ! -n $filesystem_name ]; then
    # Do not error out if empty $filesystem_name when -l or -f option is enabled.
    if (( $show_function_count != 0 )) && (( $list_filesystems != 0 )) && (( $show_summary != 0 )); then
	echo "ERROR: A filesystem name (-n) is required. The script will exit.";
	exit 1;
    fi
else
    # Verify the filesystem name exists in the glocktop file.
    filesystem_name_exists=1;
    mapfile -t fs_names < <( list_filesystems $path_to_file )
    for fs_name in "${fs_names[@]}"; do
	if [[ "$fs_name" == "$filesystem_name" ]]; then
	    filesystem_name_exists=0;
	fi
    done
    if (( $filesystem_name_exists != 0 )); then
	echo "ERROR: The filesystem name \"$filesystem_name\" was not found in the file: $path_to_file";
	exit 1;
    fi
fi

# ####################################################################
# Perform operations
# ####################################################################
# List the filesystem names that are in the glocktop files.
if (( $list_filesystems == 0 )); then
    echo "The filesystem names in $path_to_file:";
    mapfile -t fs_names < <( list_filesystems $path_to_file )
    for fs_name in "${fs_names[@]}"; do
	echo "  - $fs_name";
    done
    exit;
fi

# Show the count of functions that processes are in.
if (( $show_function_count == 0 )); then
    show_function_count $path_to_file $filesystem_name;
    exit;
fi

# Show summary of data within the glocktop file.
if (( $show_summary == 0 )); then
    show_summary $path_to_file $filesystem_name;
    exit;
fi


# Print the parsed data. The "sed" command will add a new line before each @ so that we have seperation for our
# first "awk" command. The second awk command add prefix spacing to each line.
show_glocks $path_to_file $filesystem_name;
exit;
