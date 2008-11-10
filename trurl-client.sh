#! /bin/bash

if [ x"$HOST" = x ]; then
    echo "Please set the HOST variable to a *unique* hostname."
    echo "eg. HOST=demitar-myhost"
    exit 1
fi

export PATH="/usr/lib/ccache:$PATH"
(
    flock -x -w 3300 200 && (
	curl -s -S -f "http://yellow.worldforge.org/trurl/client.php?action=bootstrap&archetype=posix&host=""$HOST" -o bootstrap.sh && bash bootstrap.sh
    )
) 200>trurl.lock
