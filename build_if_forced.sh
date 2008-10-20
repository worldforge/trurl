#! /bin/bash
(
    # wait 30 seconds for the lock (change if run at another interval)
    flock -x -w 30 210 && (
        cd /home/trurl/work/
        (test -f force/build || test -f force/update) && (./build_if_cvs_changed.sh)
    )
) 210>/home/trurl/work/force/.lock
