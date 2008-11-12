#! /bin/bash
export PATH="/usr/lib/ccache:$PATH"
test -d /home/trurl/work/lock || mkdir -p /home/trurl/work/lock
(
    # wait 55 minutes for the lock (change if run more often than every hour)
    flock -x -w 3300 200 && (
        #cd /home/trurl/work/forge/
        #cvs -z6 update > ../cvs.forge.update 2>&1
	REPOS='cvs%:pserver:cvsanon@cvs.worldforge.org:2401/home/cvspsrv/worldforge%forge cvs%:pserver:cvsanon@cvs.worldforge.org:2401/home/cvspsrv/worldforge%metaserver'
	for MODULE in ember sear libwfut eris; do
            REPOS="$REPOS git%git://git.worldforge.org/$MODULE.git%$MODULE"
        done
	ROOT=/home/trurl/work
	ROOT_SOURCE=$ROOT/source

        cd /home/trurl/work/
	echo "update" > global.state
	rm -f force/update

	test -d "$ROOT_SOURCE" || mkdir -p "$ROOT_SOURCE"
	rm -f $ROOT_SOURCE/*/updates
	for REPO_ in $REPOS; do
	    TYPE=`echo "$REPO_" | awk -F % '{ print $1 }'`
	    REPO=`echo "$REPO_" | awk -F % '{ print $2 }'`
	    REPO_MD5=`echo -n "$REPO" | md5sum | awk '{ print $1 }'`
	    NAME=`echo "$REPO_" | awk -F % '{ print $3 }'`
	    #echo '{'$TYPE'}'
	    #echo '{'$REPO'}'
	    #echo '{'$REPO_MD5'}'
	    #echo '{'$NAME'}'
            cd $ROOT_SOURCE
	    test -d "$ROOT_SOURCE/$TYPE/REPO_MD5" || mkdir -p "$ROOT_SOURCE/$TYPE/$REPO_MD5"
            cd $ROOT_SOURCE/$TYPE/$REPO_MD5
	    if [ ! -d "$NAME" ]; then
		case $TYPE in
		    cvs)
			cvs -z3 -d "$REPO" co forge >> $ROOT_SOURCE/$TYPE/updates 2>&1
			;;
		    svn)
			echo "fixme: add subversion support (simple)"
			;;
		    git)
			git clone "$REPO" >> $ROOT_SOURCE/$TYPE/updates 2>&1
			;;
		    *)
			echo "unknown repo type: $TYPE"
			;;
		esac
	    fi
	    # update
            cd "$ROOT_SOURCE/$TYPE/$REPO_MD5/$NAME"
	    case $TYPE in
		cvs)
		    cd "$ROOT_SOURCE/$TYPE/$REPO_MD5"
		    # experience show this to be slightly more stable in face of directory changes
		    cvs -z3 -d "$REPO" co "$NAME" >> $ROOT_SOURCE/$TYPE/updates 2>&1
		    ;;
		svn)
		    echo "fixme: add subversion support (simple)"
		    ;;
		git)
		    git pull origin >> $ROOT_SOURCE/$TYPE/updates 2>&1
		    ;;
		*)
		    echo "unknown repo type: $TYPE"
		    ;;
	    esac
	done
        cd /home/trurl/work/
	((test -f "$ROOT_SOURCE/git/updates" && grep -v '^Already up-to-date.$' "$ROOT_SOURCE/git/updates" > /dev/null) ||
        (test -f "$ROOT_SOURCE/cvs/updates" && egrep -v '^cvs (update|checkout): ' "$ROOT_SOURCE/cvs/updates" > /dev/null) ||
        test -f force/build) && (rm -f force/build; echo "build" > global.state; ./trurl_generate > /home/trurl/work/latest_build_log 2>&1)
#        test -f force/build) && (rm -f force/build; echo "build" > global.state; ./trurl_build > /home/trurl/work/latest_build_log 2>&1)
	cd /home/trurl/client/
	(./trurl-client.sh &)
        cd /home/trurl/work/
        rm -f global.state
	(./update_logs.sh &)
#	./trurl_render_log ../public_html/
    )
) 200>/home/trurl/work/lock/build.lock
