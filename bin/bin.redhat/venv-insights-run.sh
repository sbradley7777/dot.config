#!/bin/sh
PATH_TO_INSIGHTS_PLUGINS=~/gitlab/insights-plugins;


if [ -z $1 ]; then
   echo "Usage: $0 <path to sosreport>";
   exit 1;
fi
path_to_sosreport=$1;

if [ ! -d $PATH_TO_INSIGHTS_PLUGINS ]; then
    echo "The path to the insights-plugins directory does not exist: $PATH_TO_INSIGHTS_PLUGINS";
    exit 1;
fi

if [ ! -f $PATH_TO_INSIGHTS_PLUGINS/bin/activate ]; then
    echo "ERROR: Missing script: activate. The insights-plugins directory has not been initialized with virtualenv: $PATH_TO_INSIGHTS_PLUGINS.";
    exit 1;
fi

if [ ! -f $PATH_TO_INSIGHTS_PLUGINS/bin/insights-run ]; then
    echo "ERROR: Missing command: insights-run. The insights-plugins directory has not been initialized with virtualenv and installed all the requirements: $PATH_TO_INSIGHTS_PLUGINS.";
    exit 1;
fi

cd $PATH_TO_INSIGHTS_PLUGINS;
source $PATH_TO_INSIGHTS_PLUGINS/bin/activate;
insights-run -p telemetry,shared_rules $path_to_sosreport;

if [ -d $PATH_TO_INSIGHTS_PLUGINS/my_rules ]; then
    echo "";
    echo "----------------my_rules-------------------";
    echo "";
    insights-run -p my_rules $path_to_sosreport;
fi
exit;

