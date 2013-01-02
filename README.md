## How to Install
Clone the git repository:
~~~
$ mkdir ~/github
$ cd ~/github
$ git clone git://github.com/sbradley7777/dot.config.git
~~~

Create the symlinks to the following files in the github repository:
~~~
$ ln -s ~/github/dot.config/bash/dot.bashrc ~/.bashrc
$ ln -s ~/github/dot.config/bash/dot.bash_profile ~/.bash_profile
~~~

Here is one-liner that can be used to setup on all paths including the [emacs configuration files](https://github.com/sbradley7777/dot.config/tree/master/emacs.d):
~~~
$  mv -f ~/.bashrc ~/.bashrc.org; mv -f ~/.bash_profile ~/.bash_profile.org; rm -rf ~/.emacs ~/.emacs.d; ln -s ~/github/dot.config/bash/dot.bashrc ~/.bashrc; ln -s ~/github/dot.config/bash/dot.bash_profile ~/.bash_profile; ln -s ~/github/dot.config/emacs.d/dot.emacs ~/.emacs; ln -s ~/github/dot.config/emacs.d/ ~/.emacs.d; touch ~/.bash_profile.priv; touch ~/.bashrc.priv;
~~~

## How to add private configuration files
There are two files that are used to hold private configuration option. Once those files are created(by end user) then you can add in any configuration options that you want to the respective file.
* `~/.bash_profile.priv`
* `~/.bashrc.priv`

## To verify the installation
Start a new terminal session(shell) or new remote session. *Please note that some shells will not source in* `~/.bash_profile` *when opening just a new shell.*
