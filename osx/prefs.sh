#!/bin/sh
# Preferences Configurations of OSX.

#################################################################################
# Networking
#################################################################################

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
