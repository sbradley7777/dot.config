#!/bin/sh
# Usage is for converting bytes to gigabytes.
if [ -z $1 ]; then
   echo "Usage: $0 <Number of bytes>";
   exit 1
fi

echo "$1" | awk '{ sum=$1 ; hum[1024**3]="Gb";hum[1024**2]="Mb";hum[1024]="Kb"; for (x=1024**3; x>=1024; x/=1024){ if (sum>=x) { printf "%.2f %s\n",sum/x,hum[x];break } }}';
exit;
