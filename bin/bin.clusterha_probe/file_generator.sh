#!/bin/sh
## ###############################################################################
# This script will creata a bunch of directories and files under those
# directories.
#
# TODO:
# * Need to add getopts to have those VARS as options instead of having
#   to edit script.
# ###############################################################################

# The path to the directory where the directories and files will be created.
PATH_TO_DST_DIR="/mnt/gfs2/randomfiles";
# The number of directories that will be created.
DIR_COUNT=200
# The number of files that will be created under these directories.
FILE_COUNT=20000
# BlockSize of file
BLOCK_SIZE=1024
# Number of blocks in a file
BLOCK_COUNT=4

echo "Creating the random files in the directory: $PATH_TO_DST_DIR";

counter=1;
while [[ $counter -le $DIR_COUNT ]];
 do
  pathToDir=$PATH_TO_DST_DIR/$counter
  if [ ! -d "$pathToDir" ]; then
      echo Creating the directory: $pathToDir;
      mkdir -p $pathToDir;
  fi
  let "counter += 1";
done;

for dir in $PATH_TO_DST_DIR/*
do
  echo "Creating $FILE_COUNT random files for $dir if they do not exist."
  let "counter = 1";
  while [[ $counter -le $FILE_COUNT ]];
  do
    pathToRandomFile=$dir/random_file.$counter;
    if [ ! -f "$pathToRandomFile" ]; then
        echo "Creating the file: $pathToRandomFile."
        dd bs=$BLOCK_SIZE count=$BLOCK_COUNT if=/dev/zero of=$pathToRandomFile 2> /dev/null
    fi
    let "counter += 1";
  done;
done;

# Application Exit.
exit;
