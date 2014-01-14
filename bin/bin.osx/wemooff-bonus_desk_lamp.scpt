#! /usr/bin/osascript
#
# Author: Shane Bradley (shanebradley AT gmail.com)
# Description: This script turns on a WeMo device on for a specific device and
# logs to a file.
#
# The script requires the following github repo to be built and installed:
# https://github.com/iancmcc/ouimeaux
#
# TODO: Capture a status after doing the action and write to log file.

# Hint on the logger function below:
# http://hints.macworld.com/article.php?story=2004121710493371
on log_event(themessage)
  set theLine to (do shell script ¬
   "date  +'%Y-%m-%d %H:%M:%S'" as string) ¬
   & " " & themessage
  do shell script "echo " & theLine & ¬
   " >> ~/Library/Logs/WeMo-events.log"
end log_event

log_event("Turning off the WeMo for Bonus Room Desk Lamp.")

do shell script "/usr/local/bin/wemo switch \"Bonus Desk Lamp\" off"

