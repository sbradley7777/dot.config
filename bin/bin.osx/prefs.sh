#!/bin/sh
# Preferences Configurations of OSX.
# URL: http://knoopx.net/2011/10/28/os-x-lion-tweaks

#################################################################################
# General
#################################################################################
# Disable restore on logout, poweroff, reboot by unchecking the checkbox
defaults write com.apple.loginwindow TALLogoutSavesState -bool false
defaults write com.apple.loginwindow LoginwindowLaunchesRelaunchApps -bool false
#Disable resume system-wide.
defaults write NSGlobalDomain NSQuitAlwaysKeepsWindows -bool false
# Disable restore on apps.
defaults write com.apple.Preview NSQuitAlwaysKeepsWindows -bool false
defaults write com.apple.QuickTimePlayerX NSQuitAlwaysKeepsWindows -bool false
defaults write com.apple.TextEdit NSQuitAlwaysKeepsWindows -bool false
defaults write com.apple.Safari NSQuitAlwaysKeepsWindows -bool false
# Disable crash reporter.
defaults write com.apple.CrashReporter DialogType -string "none"
# Enable AirDrop over Ethernet and on unsupported Macs running Lion.
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true
# Turn Dashboard off.
defaults write com.apple.dashboard mcx-disabled -boolean YES

#################################################################################
# Finder
#################################################################################
# Display full POSIX path as Finder window title.
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
# Avoid creating .DS_Store files on network volumes.
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
# Show Path bar.
defaults write com.apple.finder ShowPathbar -bool true
# Show Status bar.
defaults write com.apple.finder ShowStatusBar -bool true
# Automatically do not open a new Finder window when a volume is mounted.
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool false
defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool false
#Show the ~/Library directory.
chflags nohidden ~/Library
#Show ~/bin directory
#chflags hidden ~/bin
