#!/bin/sh
# This command will rename the files in the current working directory.
# URL: https://bbs.archlinux.org/viewtopic.php?id=36305

ls | while read -r FILE
do
    mv -v "$FILE" `echo $FILE | tr ' ' '_' | tr -d '[{}(),\!]' | tr -d "\'" | tr '[A-Z]' '[a-z]' | sed 's/_-_/_/g'`;
done

ls;
exit;
