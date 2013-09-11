##Introduction
This git repo contains configuration files for `bash` and `emacs`. There is bin directory that contains some `bash` scripts. The `bash` configuration files are designed to work with `OSX` or `Linux`(only `Red Hat`/`Fedora` tested). In addition there is some `Red Hat High-Availability` scripts included if the package `cman` is installed.

When managing multiple physical machines, virtual machines, or operating systems it is very useful to have same configuration files installed.

##How to Install
This one-liner assumes the local git repo `~/github/dot.config` does not exist and uses an install file to create all the changes:
~~~
$ mkdir -p ~/github/dot.config; git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config; ~/github/dot.config/install.sh;
~~~

This one-liner assumes the local git repo `~/github/dot.config` does exist and uses an install file to create all the changes after pulling in latest code:
~~~
$ cd ~/github/dot.config; git pull --quiet; ~/github/dot.config/install.sh;
~~~

##How to Install on multiple hosts
Clone the repo on some host if it does not exist then run the following command below. This script uses ssh to clone or update the git repo and then installs the configuration files. **The hosts will need to have an ssh keys enabled and configured:**
~~~
$ mkdir -p ~/github/dot.config; git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config; ~/github/dot.config/install.sh; ~/github/dot.config/install_on_hosts.sh -p /root/github/dot.config -m "rh50 rh42"
~~~
