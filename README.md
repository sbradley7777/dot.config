##Introduction
This git repo contains configuration files for `bash` and `emacs`. There is bin directory that contains some `bash` scripts. The `bash` configuration files are designed to work with `OSX` or `Linux`(only `Red Hat`/`Fedora` tested).

This is very useful when there are multiple machines that need to have the same configuration files installed(such as lots of virtual machines).

##How to Install

**Clone the git repository assuming it does not already exist:**
~~~
$ mkdir -p ~/github/dot.config;
$ git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config;
~~~

**Create the symlinks to the following files in the github repository and backup the originals:**
~~~
$ mv -f ~/.bashrc ~/.bashrc.org > /dev/null 2>&1;
$ mv -f ~/.bash_profile ~/.bash_profile.org > /dev/null 2>&1;
$ ln -s ~/github/dot.config/bash/dot.bashrc ~/.bashrc;
$ ln -s ~/github/dot.config/bash/dot.bash_profile ~/.bash_profile;
~~~

**Create private bash configuration files that will be sourced in, but not part of this git repo:**
~~~
$ touch ~/.bash_profile.priv;
$ touch ~/.bashrc.priv;
~~~

**Create the symlinks to the emacs configuration files and backup the originals:**
~~~
$ mv -f ~/.emacs ~/.emacs.org > /dev/null 2>&1;
$ mv -f ~/.emacs.d ~/.emacs.org > /dev/null 2>&1;
$ ln -s ~/github/dot.config/emacs.d/dot.emacs ~/.emacs;
$ ln -s ~/github/dot.config/emacs.d/ ~/.emacs.d;
~~~

##How to Install with one-liner
**This one-liner assumes the local git repo `~/github/dot.config` does not exist:**
~~~
$ mkdir -p ~/github/dot.config; git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config; mv -f ~/.bashrc ~/.bashrc.org > /dev/null 2>&1; mv -f ~/.bash_profile ~/.bash_profile.org > /dev/null 2>&1; ln -s ~/github/dot.config/bash/dot.bashrc ~/.bashrc; ln -s ~/github/dot.config/bash/dot.bash_profile ~/.bash_profile; touch ~/.bash_profile.priv; touch ~/.bashrc.priv; mv -f ~/.emacs ~/.emacs.org > /dev/null 2>&1; mv -f ~/.emacs.d ~/.emacs.org > /dev/null 2>&1; ln -s ~/github/dot.config/emacs.d/dot.emacs ~/.emacs; ln -s ~/github/dot.config/emacs.d/ ~/.emacs.d;
~~~

**This one-liner assumes the local git repo `~/github/dot.config` does not exist and uses an install file to create all the changes:**
~~~
$ mkdir -p ~/github/dot.config; git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config; ~/github/dot.config/install.sh;
~~~

##How to Install on multiple hosts
Clone the repo on some host if it does not exist then run the following command below. This script uses ssh to clone or update the git repo and then installs the configuration files. **The hosts will need to have an ssh keys enabled and configured:**
~~~
$ mkdir -p ~/github/dot.config; git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config; ~/github/dot.config/install.sh; ~/github/dot.config/install_on_hosts.sh -p /root/github/dot.config -m "rh50 rh42"
~~~

##TODO
- Upload other misc generic bash scripts.
- Upload OS specific bash scripts(such as rpmsource.sh).
