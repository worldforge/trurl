#! /bin/bash

#     Copyright 2008 Anders Petersson
#
#     This file is part of Trurl.
#
#     Trurl is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     Trurl is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with Trurl.  If not, see <http://www.gnu.org/licenses/>.

HTTP_ROOT=@@HTTP_ROOT@@
HOST=@@HOST@@

CURL="curl -s -S -f"

ROOT=`pwd`/work

rm -rf "$ROOT"
mkdir -p "$ROOT"
cd "$ROOT"

function error {
  ERR=$?
  echo "Error"
  echo "Host: $1"
  echo "Snapshot: $2"
  echo "Build: $3"
  echo "Error: $4"
  echo "Message: $5"
  if [ x"$6" = x ]; then
    $CURL --data-urlencode "action=error" --data-urlencode "host=$1" --data-urlencode "snapshot=$2" --data-urlencode "build=$3" --data-urlencode "error=$4" --data-urlencode "message=$5" "$HTTP_ROOT"
  else
    echo
    echo "Extra:"
    cat "$6"
    $CURL --data-urlencode "action=error" --data-urlencode "host=$1" --data-urlencode "snapshot=$2" --data-urlencode "build=$3" --data-urlencode "error=$4" --data-urlencode "message=$5" --data-urlencode "extra@$6" "$HTTP_ROOT"
  fi
  return $ERR
}

function ready {
  if [ -f system.tar ]; then
      $CURL --data-urlencode "action=ready" --data-urlencode "archetype=posix" --data-urlencode "host=$1" --data-urlencode "platform@$2" --data-urlencode "system@system.tar" -o ready.sh "$HTTP_ROOT"
  else
      $CURL --data-urlencode "action=ready" --data-urlencode "archetype=posix" --data-urlencode "host=$1" --data-urlencode "platform@$2" -o ready.sh "$HTTP_ROOT"
  fi
}

(
    (lsb_release -a > platform 2>&1 || error "$HOST" '' '' 'bootstrap.platform' 'lsb_release -a' platform) &&
    (echo -n 'uname -m: ' >> platform; uname -m >> platform 2>&1 || error "$HOST" '' '' 'bootstrap.platform' 'uname -m' platform) &&
    (echo -n 'uname -o: ' >> platform; uname -o >> platform 2>&1 || error "$HOST" '' '' 'bootstrap.platform' 'uname -o' platform) &&
    (which dpkg > /dev/null 2>&1 && ((dpkg -l > system.dpkg 2>&1 && (test -f system.tar && tar rf system.tar system.dpkg || tar cf system.tar system.dpkg)) || error "$HOST" '' '' 'bootstrap.system' 'dpkg -l' system.dpkg); true) &&
    # try to avoid errors when yum is installed to facilitate chroots
    ((if which dpkg > /dev/null 2>&1; then false; else true; fi) && which yum > /dev/null 2>&1 && ((yum list installed > system.yum 2>&1 && (test -f system.tar && tar rf system.tar system.yum || tar cf system.tar system.yum)) || error "$HOST" '' '' 'bootstrap.system' 'yum list installed' system.yum); true) &&
    (ready "$HOST" platform || error "$HOST" '' '' 'bootstrap.ready.download' '' platform) &&
    (LC_ALL=C bash ready.sh || error "$HOST" '' '' 'bootstrap.ready.run' '' ready.sh)
)
