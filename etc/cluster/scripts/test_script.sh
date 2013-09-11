#!/bin/bash
#
# ###############################################################################
# This script simply creates and remove a file. It is used as example
# /usr/share/cluster/script.sh resource example.

# START:  Creates the lock file
# STOP:   Removes the lock file
# STATUS: Checks if the lock file exists
# ###############################################################################
. /etc/init.d/functions

SCRIPTNAME=${0##*/}
LOCKFILE=/var/lock/subsys/$SCRIPTNAME
# 0 is enabled(true) and 1 is disabled(false)
DELAY_STARTUP_SECONDS=0;

# ###############################################################################
# The clug utility uses the normal logging levels as defined in
# sys/syslog.h. Calls to clulog will use the logging level defined
# for the Service Manager (clusvcmgrd).
# ###############################################################################
LOG_EMERG=0 # system is unusable
LOG_ALERT=1 # action must be taken immediately
LOG_CRIT=2 # critical conditions
LOG_ERR=3 # error conditions
LOG_WARNING=4 # warning conditions
LOG_NOTICE=5 # normal but significant condition
LOG_INFO=6 # informational
LOG_DEBUG=7 # debug-level messages

control_c() {
  # Run if user hits control-c so clean exit.
  echo -en "\n*** Control-c and will exit.***\n"
  clulog -s $LOG_INFO "Control-c caught and will stop the script $0".
  stopService;
  exit $?
}

usage() {
    echo $"usage: $SCRIPTNAME {start|stop|restart|reload|status}"
    exit 1
}

stopService() {
    clulog -s $LOG_INFO "Executing stop on the script: $0"
	if [ ! -f $LOCKFILE ]; then
        clulog -s $WARNING "The script is not running: $0."
        exit 0;
    fi
    # If the orginal start process is still running then kill that process.
    pid=`cat $LOCKFILE`
    kill -9 $pid &> /dev/null
    # If there was no error on killing the process then remove the file.
    [ $? ] && rm $LOCKFILE; exit $? || exit 1;
}

startService() {
    clulog -s $LOG_INFO "Executing start on the script: $0"
    echo $$ > $LOCKFILE
    result=$?
    # A loop that will keep service in "starting" state.
	if [ "$DELAY_STARTUP_SECONDS" -gt 0 ]; then
        clulog -s $LOG_INFO "Delaying startup by $DELAY_STARTUP_SECONDS seconds for the script: $0."
        sleep $DELAY_STARTUP_SECONDS;
    fi
    exit $result;
}

statusService() {
    clulog -s $LOG_INFO "Executing status on the script: $0"
	if [ -f $LOCKFILE ]; then
	    clulog -s $LOG_INFO "The script $SCRIPTNAME is running."
	    exit 0;
	else
	    clulog -s $LOG_INFO "The script $SCRIPTNAME is not running."
	    exit 1;
	fi
}
reloadService() {
    clulog -s $LOG_INFO "Executing reload on the script: $SCRIPTNAME.";
}

# ###############################################################################
# Main
# ###############################################################################

# trap keyboard interrupt (control-c)
trap control_c SIGINT

case "$1" in
  start)   $0 stop; startService; ;;
  stop)    stopService; ;;
  status)  statusService; ;;
  restart) $0 stop; startService; ;;
  reload)  reloadService; ;;
  *) usage; ;;
esac

exit 0
