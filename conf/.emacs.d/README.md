##Introduction
The `emacs.d` collection of files contains the main `.emacs` configuration file and the site-lisp directory that contains some .el files used by the main configuration file.

## How to install
Backup the orginal configuration file and directory if there was one:
~~~
$ mv ~/.emacs.d ~/.emacs.d.org
$ mv ~/.emacs ~/.emacs.org
$ mv ~/.emacs.elc ~/.emacs.elc.org
~~~

Create the symlinks to the following files in the github repository:
~~~
$ ln -s ~/github/dot.config/emacs.d/dot.emacs ~/.emacs
$ ln -s ~/github/dot.config/emacs.d/ ~/.emacs.d
~~~

##The autosaves and backup files
The autosaves files which are files with *~* and backup files which are files with *#* will be saved to the following locations below:

* The autosave files will be located in this directory: `~/.emacs_archive/autosaves/`
* The backup files will be located in this directory: `~/.emacs_archive/backups/`

The directories will be automatically created if they do not exist when you start emacs and an autosave file or backup file needs to be saved.


