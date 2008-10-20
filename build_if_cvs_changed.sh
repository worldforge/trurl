#! /bin/bash
export PATH="/usr/lib/ccache:$PATH"
(
    # wait 55 minutes for the lock (change if run more often than every hour)
    flock -x -w 3300 200 && (
        #cd /home/trurl/work/forge/
        #cvs -z6 update > ../cvs.forge.update 2>&1
        cd /home/trurl/work/
        echo "update" > global.state
        rm -f force/update
	cvs -z3 -d :pserver:cvsanon@cvs.worldforge.org:2401/home/cvspsrv/worldforge co forge > cvs.forge.update 2>&1
        for MODULE in ember sear libwfut; do
            cd /home/trurl/work/git/$MODULE/
            git pull origin > ../../git.$MODULE.update 2>&1
        done
        cd /home/trurl/work/
        ((grep -v '^Already up-to-date.$' git.*.update > /dev/null) ||
        (egrep -v '^cvs (update|checkout): ' cvs.forge.update > /dev/null) ||
        test -f force/build) && (rm -f force/build; echo "build" > global.state; ./trurl_build > /home/trurl/work/latest_build_log 2>&1)
        cd /home/trurl/work/
        rm -f global.state
	./trurl_render_log ../public_html/
    )
) 200>/home/trurl/work/build.lock
