## How to Install

###Clone the git repository assuming it does not already exist:
~~~
$ mkdir -p ~/github/dot.config;
$ git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config;
~~~

###Create the symlinks to the following files in the github repository and backup the originals:
~~~
$ mv -f ~/.bashrc ~/.bashrc.org > /dev/null 2>&1;
$ mv -f ~/.bash_profile ~/.bash_profile.org > /dev/null 2>&1;
$ ln -s ~/github/dot.config/bash/dot.bashrc ~/.bashrc;
$ ln -s ~/github/dot.config/bash/dot.bash_profile ~/.bash_profile;
~~~

###Create private bash configuration files that will be sourced in, but not part of this git repo.
~~~
$ touch ~/.bash_profile.priv;
$ touch ~/.bashrc.priv;
~~~

###Create the symlinks to the emacs configuration files and backup the originals::
~~~
$ mv -f ~/.emacs ~/.emacs.org > /dev/null 2>&1;
$ mv -f ~/.emacs.d ~/.emacs.org > /dev/null 2>&1;
$ ln -s ~/github/dot.config/emacs.d/dot.emacs ~/.emacs;
$ ln -s ~/github/dot.config/emacs.d/ ~/.emacs.d;
~~~

###To verify the installation
Start a new terminal session(shell) or new remote session. *Please note that some shells will not source in* `~/.bash_profile` *when opening just a new shell.*

###Install with one-liner. This one-liner assumes the local git repo `~/github/dot.config` does not exist:
~~~
$ mkdir -p ~/github/dot.config; git clone --quiet git://github.com/sbradley7777/dot.config.git ~/github/dot.config; mv -f ~/.bashrc ~/.bashrc.org > /dev/null 2>&1; mv -f ~/.bash_profile ~/.bash_profile.org > /dev/null 2>&1; ln -s ~/github/dot.config/bash/dot.bashrc ~/.bashrc; ln -s ~/github/dot.config/bash/dot.bash_profile ~/.bash_profile; touch ~/.bash_profile.priv; touch ~/.bashrc.priv; mv -f ~/.emacs ~/.emacs.org > /dev/null 2>&1; mv -f ~/.emacs.d ~/.emacs.org > /dev/null 2>&1; ln -s ~/github/dot.config/emacs.d/dot.emacs ~/.emacs; ln -s ~/github/dot.config/emacs.d/ ~/.emacs.d;
~~~

