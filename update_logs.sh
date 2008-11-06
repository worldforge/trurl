#! /bin/bash
#(test -f force/render) &&
(
    flock -x -w 600 200 && (
	umask 022
	cd /home/trurl/work/
	(rm -f force/render; ./trurl_render_log ../public_html/)
    )
) 200>/home/trurl/work/render.lock
