# How to Install
Clone the git repository:
~~~
$ mkdir ~/github
$ cd ~/github
$ git clone git://github.com/sbradley7777/dot.config.git
~~~

Create symlinks to the following files:
$ ln -s ~/github/dot.config/bash/dot.bashrc ~/.bashrc
$ ln -s ~/github/dot.config/bash/dot.bash_profile ~/.bash_profile

# How to add private configuration files
Private bash_profile configuration options are located in this configuration
file: $HOME/.bash_profile.priv
~~~
$ touch ~/.bash_profile.priv
~~~

Private bash_profile configuration options are located in this configuration
file: $HOME/.bashrc.priv
~~~
$ touch ~/.bashrc.priv
~~~
