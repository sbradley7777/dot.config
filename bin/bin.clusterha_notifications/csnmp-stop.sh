#!/bin/sh
service corosync-notifyd start
service foghorn start
service snmpd start
service messagebus restart
