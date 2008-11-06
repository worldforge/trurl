#! /bin/bash

if ! [ -f trurl-client.sh ]; then
    wget http://yellow.worldforge.org/trurl/trurl-client.sh
    chmod +x trurl-client.sh
fi

echo "Running the trurl-client script every 10 minutes."
echo
echo -n "begin "; date
while (VERBOSE=1 ./trurl-client.sh); do echo -n "end "; date; echo; sleep 10m; echo -n "begin "; date; done
