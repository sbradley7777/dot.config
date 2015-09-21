##Introduction
The script `gfs2_lockdump_count_hw.sh` is used to analyze GFS2 lockdumps and `glocktop` output from files and the analysis will be written to standard out. The example below will look for any glock that has `8` or more holder+waiters on a glock or it will log any glock with demote time higher than 0.
~~~
$ gfs2_lockdump_count_hw.sh -m 8 -p /tmp/glocktop.output.rh6node1.examplerh.com
~~~
