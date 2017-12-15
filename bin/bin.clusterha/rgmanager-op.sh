#!/bin/sh

usage() {
    echo "ERROR: No action given. Valid operations are: stop, start.";
}
stop() {
    for cn in $(grep "clusternode name" /etc/cluster/cluster.conf | cut -d '"' -f 2); do
	echo "Stopping rgmanager on $cn";
	ssh $cn "service rgmanager stop";
	sleep 2;
    done;
}
start() {
    for cn in $(grep "clusternode name" /etc/cluster/cluster.conf | cut -d '"' -f 2); do
	echo "Starting rgmanager on $cn";
	ssh $cn "service rgmanager start";
    done;
}

rc=0;
case "$1" in
  stop)    stop;    rc=$?; ;;
  start)   start;   rc=$?; ;;
  *) rc=3; usage;;
esac
exit $rc;
