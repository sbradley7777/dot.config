#!/bin/sh
PATH_TO_INSIGHTS_PLUGINS=~/gitlab/insights-plugins;


if [ -z $1 ]; then
   echo "Usage: $0 <path to sosreport>";
   exit 1;
fi
path_to_sosreport=$1;

cd $PATH_TO_INSIGHTS_PLUGINS;
source $PATH_TO_INSIGHTS_PLUGINS/bin/activate;
insights-run -p telemetry,shared_rules $path_to_sosreport
exit;

