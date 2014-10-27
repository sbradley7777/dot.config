#!/bin/sh
# Preferences Configurations of OSX.
# Main osx-for-hackers.sh: https://gist.github.com/brandonb927/3195465
# Another example for osx-for-hackers.sh: https://github.com/diimdeep/dotfiles/blob/master/osx/configure/yosemite/osx_set_defaults.sh
# Another example for osx-for-hackers.sh: https://github.com/diimdeep/dotfiles/blob/master/osx/configure/yosemite/osx_set_defaults.sh
# Another example for osx-for-hackers.sh: https://gist.github.com/MatthewMueller/e22d9840f9ea2fee4716
# Useful examples and what osx-for-hackers.sh based on: https://github.com/mathiasbynens/dotfiles/blob/master/.osx
# Yosemite hiddle utilies link: https://github.com/diimdeep/dotfiles/blob/master/osx/configure/yosemite/link_hidden_utilities
# OSX Lion Tweaks: http://knoopx.net/2011/10/28/os-x-lion-tweaks


# Set the colours you can use
black='\033[0;30m'
white='\033[0;37m'
red='\033[0;31m'
green='\033[0;32m'
yellow='\033[0;33m'
blue='\033[0;34m'
magenta='\033[0;35m'
cyan='\033[0;36m'


#  Reset text attributes to normal + without clearing screen.
alias Reset="tput sgr0"

# Color-echo.
# arg $1 = message
# arg $2 = Color
cecho() {
  echo "${2}${1}"
  Reset # Reset to normal.
  return
}

# Set continue to false by default
CONTINUE=false

echo ""
cecho "###############################################" $red
cecho "#        DO NOT RUN THIS SCRIPT BLINDLY       #" $red
cecho "#         YOU'LL PROBABLY REGRET IT...        #" $red
cecho "#                                             #" $red
cecho "#              READ IT THOROUGHLY             #" $red
cecho "#         AND EDIT TO SUIT YOUR NEEDS         #" $red
cecho "###############################################" $red
echo ""

echo ""
cecho "Have you read through the script you're about to run and " $red
cecho "understood that it will make changes to your computer? (y/n)" $red
read -r response
case $response in
  [yY]) CONTINUE=true
      break;;
  *) break;;
esac

if ! $CONTINUE; then
  # Check if we're continuing and output a message if not
  cecho "Please go read the script, it only takes a few minutes" $red
else
  # Here we go.. ask for the administrator password upfront and run a
  # keep-alive to update existing `sudo` time stamp until script has finished
  sudo -v
  while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

  ###############################################################################
  # General UI/UX
  ###############################################################################

  echo ""
  echo "Would you like to set your computer name (as done via System Preferences >> Sharing)?  (y/n)"

fi
exit;
