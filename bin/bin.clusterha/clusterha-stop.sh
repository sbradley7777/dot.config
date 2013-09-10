#!/bin/sh
# This script will stop all the cluster services if they exist in /etc/init.d.

SCRIPT_LOC=/etc/init.d
SERVICES=( rgmanager gfs2 gfs scsi_reserve qdiskd clvmd cmirror fenced lock_gulmd cman ccsd  );

for i in ${!SERVICES[@]}
  do
    servicei=${SERVICES[i]};
    if [ -f $SCRIPT_LOC/$servicei ]
    then
        $SCRIPT_LOC/$servicei stop
    fi
done
