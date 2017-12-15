#!/bin/sh
if [ -z $1 ]; then
   echo "Usage: $0 <filename>"
   exit 1
fi
path_to_cluster_conf="$1"
path_to_script_dir="/etc/init.d-rgmanager";
path_to_base_script="/etc/init.d/lsb-resource";

if [ ! -d $path_to_script_dir ]; then
    mkdir $path_to_script_dir;
fi

if [ ! -f $path_to_base_script ]; then
    echo "ERROR: Base script does not exist: $path_to_base_script";
    exit 1;
fi

for path_to_script in $(grep script $path_to_cluster_conf | grep -v passwd_script | awk '{print $2}' | cut -d '"' -f2 | sort | uniq); do
    script=$(basename $path_to_script);
    path_to_copied_script="$path_to_script_dir/$script";
    if [ ! -f path_to_copied_script ]; then
	cp $path_to_base_script $path_to_copied_script;
    fi
done;
