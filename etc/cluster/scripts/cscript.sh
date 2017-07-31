#!/bin/bash
# lsb-resource:        Simple LSB script resource.
#
# chkconfig: 345 97 03
# description: Simple LSB script resource.
# processname:
# pidfile: /var/run/lsb-resource.pid

### BEGIN INIT INFO
# Required-Start: $local_fs $network $remote_fs $named $time
# Required-Stop: $local_fs $network $remote_fs $named
# Default-Start: 2 3 4 5
# Default-Stop: 0 1 6
### END INIT INFO

# ###############################################################################
# This script simply creates and remove a file. It is used as example
# /usr/share/cluster/script.sh resource example.

# START:  Creates the lock file
# STOP:   Removes the lock file
# STATUS: Checks if the lock file exists
# ###############################################################################

# Will cause errors to occur if this function is source in.
#. /etc/init.d/functions

# Name of the logger
LOGGER_NAME=`basename $0`;

# Path to the lockfile
LOCKFILE=/var/lock/subsys/${0##*/}

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

clulog(){
    logger_level="local7.info";
    case "$1" in
	$LOG_INFO)  logger_level="local7.info"; ;;
	$LOG_WARNING)  logger_level="local7.warning"; ;;
	*)  logger_level="local7.info"; ;;
    esac
    logger -i -t "$LOGGER_NAME" -p "$logger_level" "$2"
}

control_c() {
  # Run if user hits control-c so clean exit.
  echo -en "\n*** Control-c and will exit.***\n"
  clulog $LOG_INFO "Control-c caught and will stop the script $0".
  stopService;
  exit $?
}

usage() {
    echo $"usage: $SCRIPTNAME {start|stop|restart|reload|status}"
    exit 1
}

stopService() {
    clulog $LOG_INFO "Executing stop on the script: $0"
    if [ ! -f $LOCKFILE ]; then
        clulog $LOG_WARNING "The script is not running: $0."
        exit 0;
    fi
    # If the orginal start process is still running then kill that process.
    pid=`cat $LOCKFILE`
    kill -9 $pid &> /dev/null
    # If there was no error on killing the process then remove the file.
    [ $? ] && rm $LOCKFILE; exit $? || exit 1;
}

startService() {
    clulog $LOG_INFO "Executing start on the script: $0"
    echo $$ > $LOCKFILE
    result=$?
    # A loop that will keep service in "starting" state.
	if [ "$DELAY_STARTUP_SECONDS" -gt 0 ]; then
        clulog $LOG_INFO "Delaying startup by $DELAY_STARTUP_SECONDS seconds for the script: $0."
        sleep $DELAY_STARTUP_SECONDS;
    fi
    exit $result;
}

statusService() {
    basename=${0##*/}
    clulog $LOG_INFO "Executing status on the script: $0"
	if [ -f $LOCKFILE ]; then
	    clulog $LOG_INFO "The script $basename is running."
	    exit 0;
	else
	    clulog $LOG_INFO "The script $basename is not running."
	    exit 1;
	fi
}
reloadService() {
    clulog $LOG_INFO "Executing reload on the script: $SCRIPTNAME.";
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
