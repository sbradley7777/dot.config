#!/bin/sh
PATH_TO_INSIGHTS_PLUGINS=~/gitlab/insights-plugins;

usage() {
    bname=$(basename $0);
    echo -e "usage: $bname -p <path to file> -I -G -M";
    echo -e "This script runs insights-run command against the rules for: insights-plugins, gss-rules, my_rules";
    echo "OPTIONS:";
    echo "   -h      show this message";
    echo "   -p      path to the sosreport";
    echo "   -I      disable the insights-plugins rules";
    echo "   -G      disable the gss-rules rules";
    echo "   -M      disable the my_rules rules (your local rules)";
    echo -e "\nEXAMPLE:";
    echo "Analyze an sosreport and disable the rules for insights-plugins and gss-rules.";
    echo "$ $bname -p ~/sosreports-insights/sosreport-rhel7-1-2019-12-06-mhxylet.tar.xz -I -G";
}


disable_insights_plugins=0;
disable_gss_rules=0;
disable_my_rules=0;
while getopts ":hp:IGM" opt; do
    case $opt in
	h)
            usage;
            exit;
            ;;
	p)
            path_to_sosreport=$OPTARG;
	    ;;
	I)
            disable_insights_plugins=1;
            ;;
	G)
            disable_gss_rules=1;
            ;;
	M)
            disable_my_rules=1;
            ;;
	\?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
	:)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done

if [ -z $path_to_sosreport ]; then
   echo "Usage: $0 <path to sosreport>";
   exit 1;
fi

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

# Configure the environment.
cd $PATH_TO_INSIGHTS_PLUGINS;
source $PATH_TO_INSIGHTS_PLUGINS/bin/activate;
# Run the insights-cmd against any enabled rules.
if [[ disable_insights_plugins -ne 1 || disable_gss_rules -ne 1 ]]; then
    # Enable and disable plugins.
    plugins="";
    if [[ disable_insights_plugins -ne 1 ]]; then
	plugins="telemetry";
    fi
    if [[ disable_gss_rules -ne 1 ]]; then
	if [[ -z $plugins ]]; then
	    plugins="shared_rules";
	else
	    plugins="$plugins,shared_rules";
	fi
    fi
    insights-run -p $plugins $path_to_sosreport;
fi

if [[ disable_my_rules -ne 1 ]]; then
    if [ -d $PATH_TO_INSIGHTS_PLUGINS/my_rules ]; then
	echo "";
	echo "----------------my_rules-------------------";
	echo "";
	insights-run -p my_rules $path_to_sosreport;
    fi
fi
exit;

