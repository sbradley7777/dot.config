#!/bin/sh
# This script will start all the cluster services if they exist in /etc/init.d.

SCRIPT_LOC=/etc/init.d
SERVICES=( ccsd cman fenced clvmd qdiskd gfs gfs2 rgmanager );
# Comment in cmirror or lock_gulmd needs to start
# SERVICES=( ccsd cman lock_gulmd fenced cmirror clvmd qdiskd gfs gfs2 rgmanager );

for i in ${!SERVICES[@]}
  do
    servicei=${SERVICES[i]};
    if [ -f $SCRIPT_LOC/$servicei ]
    then
        $SCRIPT_LOC/$servicei start
    fi
done
