#!/bin/sh
# This script will log rgmnager snmp traps.

# #####################################################################
# Global Vars:
# #####################################################################
# The log directory that will contain the snmp traps that are logged
# to a file if enabled.
LOG_DIR="/var/log/snmptrapd-traps";
# Path to the file that will be used to log the snmp trap if enabled.
PATH_TO_LOG_FILE="$LOG_DIR/rgmanager.log";
# If 0 this snmp trap will be log to a file, if 1 it will not be
# logged to a file.
ENABLE_LOGGING_TO_FILE=0;

# #####################################################################
# Define the vars that are in the snmp trap:
# #####################################################################
# All the default values in the trap.
sysUpTimeInstance="";
snmpMIBName="";
snmpTrapName="";

# All the the defined values in the trap.
rgmanagerServiceName="";
rgmanagerServiceState="";
rgmanagerServiceFlags="";
rgmanagerServiceCurrentOwner="";
rgmanagerServicePreviousOwner="";

# #####################################################################
# Process the snmp trap:
# #####################################################################
# Read in the hostname string.
read host;
srcHostname=$(echo $host | cut -d'=' -f 2);

# Read in the address string.
# addr=UDP: [192.168.1.161]:60372->[192.168.1.161]
read addr;

while read oid val
do
    #logger $oid
    if [ "$oid" = "DISMAN-EVENT-MIB::sysUpTimeInstance" ]; then
        sysUpTimeInstance=$val
    elif [ "$oid" = "SNMPv2-MIB::snmpTrapOID.0" ]; then
        snmpMIBName=$(echo $val | cut -d':' -f 1);
        snmpTrapName=$(echo $val | cut -d':' -f 3);
    elif [ "$oid" = "$snmpMIBName::rgmanagerServiceName.0" ]; then
          rgmanagerServiceName=$val
    elif [ "$oid" = "$snmpMIBName::rgmanagerServiceState.0" ]; then
          rgmanagerServiceState=$val
    elif [ "$oid" = "$snmpMIBName::rgmanagerServiceFlags.0" ]; then
          rgmanagerServiceFlags=$val
    elif [ "$oid" = "$snmpMIBName::rgmanagerServiceCurrentOwner.0" ]; then
          rgmanagerServiceCurrentOwner=$val
    elif [ "$oid" = "$snmpMIBName::rgmanagerServicePreviousOwner.0" ]; then
          rgmanagerServicePreviousOwner=$val
    fi
done

# Log to a file if that is enabled.
if [ $ENABLE_LOGGING_TO_FILE -eq 0 ]; then 
    # Create the log directory if it does not exist
    if [ ! -d "$LOG_DIR" ]; then
        mkdir -p $LOG_DIR;
    fi;
    echo "------------------------------------------------" >> $PATH_TO_LOG_FILE;
    echo "srcHostname = $srcHostname" >> $PATH_TO_LOG_FILE;
    echo "addr=$addr" >> $PATH_TO_LOG_FILE;
    echo "    rgmanagerServiceName = $rgmanagerServiceName" >> $PATH_TO_LOG_FILE;
    echo "    rgmanagerServiceState = $rgmanagerServiceState" >> $PATH_TO_LOG_FILE;
    echo "    rgmanagerServiceFlags = $rgmanagerServiceFlags" >> $PATH_TO_LOG_FILE;
    echo "    rgmanagerServiceCurrentOwner = $rgmanagerServiceCurrentOwner" >> $PATH_TO_LOG_FILE;
    echo "    rgmanagerServicePreviousOwner = $rgmanagerServicePreviousOwner" >> $PATH_TO_LOG_FILE;
    echo "------------------------------------------------" >> $PATH_TO_LOG_FILE;
    echo "" >> $PATH_TO_LOG_FILE;
fi

# Create a message.
message="$srcHostname sent the following trap $snmpMIBName::$snmpTrapName";
message="$message | The cluster service $rgmanagerServiceName had a state change to $rgmanagerServiceState with flags $rgmanagerServiceFlags.";
message="$message The previous owner of the service was $rgmanagerServicePreviousOwner and the current owner of the service is $rgmanagerServiceCurrentOwner."

# Mail the message.
echo "$message" | mail -s  "Cluster Service $rgmanagerServiceName State Change Alert" root@localhost

# Exit the script.
exit;
