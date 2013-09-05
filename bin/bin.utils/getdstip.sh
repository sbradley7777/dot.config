#!/bin/sh

# This script will get the ip for an ethernet device for some host.

ETHERNET_DEVICE="eth0";

host=$1;
ping -q -c 1 $host &>/dev/null
if [ $? -eq 0 ] ; then
    #ipAddr=$(ssh $host  "/sbin/ifconfig $ETHERNET_DEVICE | grep 'inet addr:' | cut -d: -f2 " | awk '{ print $1}');
    ipAddr=$(ssh $host  "ip addr show dev $ETHERNET_DEVICE | sed -e's/^.*inet \([^ ]*\)\/.*$/\1/;t;d'");
    echo "$ipAddr";
    exit;
else
    echo "";
    exit 1;
fi
exit
