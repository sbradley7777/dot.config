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

## How to add private configuration files
There are two files that are used to hold private configuration option. Once those files are created(by end user) then you can add in any configuration options that you want to the respective file.
* ~/.bash_profile.priv
* ~/.bashrc.priv
