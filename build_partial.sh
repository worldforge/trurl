#! /bin/bash
export PATH="/usr/lib/ccache:$PATH"
test -d /home/trurl/work/lock || mkdir -p /home/trurl/work/lock
(
    flock -x -w 10 200 && (
        cd /home/trurl/work/
        ./trurl_build $@
    )
) 200>/home/trurl/work/lock/build.lock
