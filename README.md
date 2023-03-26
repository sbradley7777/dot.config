##Introduction
This git repo contains configuration files for `bash` and `emacs`. There is bin directory that contains some `bash` scripts. The `bash` configuration files are designed to work with `OSX` or `Linux`(only `Red Hat`/`Fedora` tested). In addition there is some `Red Hat High-Availability` scripts included but are installed by default(see configuration file section below).
When managing multiple physical machines, virtual machines, or operating systems it is very useful to have same configuration files installed.



##How to Install
This one-liner assumes the local git repo `~/github/dot.config` does not exist and uses an install file to create all the changes:
~~~
$ mkdir -p ~/github/dot.config; git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config; ~/github/dot.config/install.py -y;
~~~

This one-liner assumes the local git repo `~/github/dot.config` does exist and uses an install file to create all the changes after pulling in latest code:
~~~
$ cd ~/github/dot.config; git pull --quiet; ~/github/dot.config/install.py -y;
~~~

##Configuration File for Installer
A configuration file can be used to override default files that will be installed or add files that are in the repo but were not installed. The installer will check if this file exists when it is ran. For Example:
~~~
# cat ~/.dot.config
[bin.clusterha]
src_path = bin/bin.clusterha
dst_path = ~/bin/bin.clusterha
platform = Linux

[bin.clusterha_probe]
src_path = bin/bin.clusterha_probe
dst_path = ~/bin/bin.clusterha_probe
platform = Linux

[test_script.sh]
src_path = etc/cluster/scripts/test_script.sh
dst_path = /etc/cluster/scripts/test_script.sh
platform = Linux
~~~

This configuration will install a couple of files or a directory of files for any host running a Linux kernel. This example is the one that is used for Linux cluster nodes. The word *config file* means a file or directory.

- `[some unique name]`: the section header should be a unique name to describe the config file.
- `src_path`: is relative path to the git repo. If there is no value then an empty file will be created if the file does not exist.
- `dst_path`: is the absolute path to the where the file will be installed.
- `platform`: is the name of the host's platform returned by `platform.system()`. If there is not value set for `platform` then the file can be installed on *all* platforms.

##How to Install on multiple hosts
Clone the repo on some host if it does not exist then run the following command below. This script uses ssh to clone or update the git repo and then installs the configuration files. **The hosts will need to have an ssh keys enabled and configured:**
~~~
$ mkdir -p ~/github/dot.config; git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config; ~/github/dot.config/install.py -y ; ~/github/dot.config/install_on_hosts.sh -p /root/github/dot.config -m "rh50 rh42"
~~~


##References
* [bash configuration files and functions](https://github.com/mathiasbynens/dotfiles)
