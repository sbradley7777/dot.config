#!/bin/sh
######################################################################
# net_breaker.sh
#
# Author: Shane Bradley
# Description: This script will simulate a network cable for a cluster.
#
# Takes 1 argument to block the ip address and multicast address.
# If no arguments are given then it flushes iptables.
# usage: net_breaker.sh <ip of host to block>
#        net_breaker.sh
#
# Based on the following script:
# - https://github.com/corosync/corosync/blob/master/cts/agents/net_breaker.sh
######################################################################
if [ -z $1 ]; then
  iptables -F >/dev/null 2>&1
else
  iptables -A INPUT -s $1 -j DROP >/dev/null 2>&1
  iptables -A OUTPUT -s $1 -j DROP >/dev/null 2>&1
  iptables -A INPUT -m pkttype --pkt-type multicast -j DROP
fi

exit 0

