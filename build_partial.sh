#! /bin/bash
export PATH="/usr/lib/ccache:$PATH"
(
    flock -x -w 10 200 && (
        cd /home/trurl/work/
        ./trurl_build $@
    )
) 200>/home/trurl/work/build.lock
