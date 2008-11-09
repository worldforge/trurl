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
HTTP_SOURCE_ROOT=@@HTTP_SOURCE_ROOT@@
HOST=@@HOST@@
SNAPSHOT=@@SNAPSHOT@@
BUILD=@@BUILD@@

CURL="curl -s -S -f"

ROOT=`pwd`
BUILD_ROOT="$ROOT"/build
TARGET="$ROOT"/target

mkdir -p "$ROOT"
mkdir -p "$TARGET"
mkdir -p "$TARGET"/share/aclocal
cd "$ROOT"

function checkpoint {
  if [ x"$5" = x ]; then
    if [ x"$VERBOSE" != x ]; then
      echo "$1" "$2" "$3" "$4"
    fi
    $CURL --data-urlencode "action=checkpoint" --data-urlencode "host=$1" --data-urlencode "snapshot=$2" --data-urlencode "build=$3" --data-urlencode "edge=$4" "$HTTP_ROOT"
  else
    if [ x"$6" = x ]; then
      if [ x"$VERBOSE" != x ]; then
	if [ x"$4" = xbegin ]; then
	  echo "  ""$5"" {"
	  echo -n "    "
	else
	  echo
	  echo "  } ""$5"
	fi
      fi
      $CURL --data-urlencode "action=checkpoint" --data-urlencode "host=$1" --data-urlencode "snapshot=$2" --data-urlencode "build=$3" --data-urlencode "edge=$4" --data-urlencode "module=$5" "$HTTP_ROOT"
    else
      if [ x"$VERBOSE" != x ]; then
	if [ x"$4" = xbegin ]; then
	  echo -n "$6"
	else
	  echo -n ", "
	fi
      fi
      $CURL --data-urlencode "action=checkpoint" --data-urlencode "host=$1" --data-urlencode "snapshot=$2" --data-urlencode "build=$3" --data-urlencode "edge=$4" --data-urlencode "module=$5" --data-urlencode "step=$6" "$HTTP_ROOT"
    fi
  fi
}

function upload {
  $CURL --data-urlencode "action=upload" --data-urlencode "host=$1" --data-urlencode "snapshot=$2" --data-urlencode "build=$3" --data-urlencode "module=$4" --data-urlencode "step=$5" --data-urlencode "log@$6" --data-urlencode "meta@$7" "$HTTP_ROOT"
}

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

(checkpoint "$HOST" "$SNAPSHOT" "$BUILD" 'begin' || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.checkpoint' 'begin')

##MODULE-BEGIN##
{
    (checkpoint "$HOST" "$SNAPSHOT" "$BUILD" 'begin' @@MODULE@@ || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.checkpoint' @@MODULE@@'.begin')
    
    cd "$ROOT" &&
    URL="$HTTP_SOURCE_ROOT"/"$SNAPSHOT"/@@module@@.tar.gz
    URL_PATCH="$HTTP_SOURCE_ROOT"/"$SNAPSHOT"/@@module@@.patch
    ($CURL -o @@module@@.tar.gz "$URL" || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.source.download' "$URL") &&
    (if [ x@@patch@@ != x ]; then ($CURL -o @@module@@.patch "$URL_PATCH" || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.patch.download' "$URL_PATCH"); fi) &&
    
    {
	mkdir -p "$BUILD_ROOT"/@@module@@ &&
	cd "$BUILD_ROOT"/@@module@@ &&
	(tar xzf "$ROOT"/@@module@@.tar.gz || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.source.unpack' "$URL") &&
	(if [ x@@patch@@ != x ]; then ((patch -p@@patch-strip@@ < "$ROOT"/@@module@@.patch > /dev/null) || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.patch.apply' "$URL_PATCH"); fi)

    } &&
    
    {
##MODULE-STEP-BEGIN##
	{
	    (checkpoint "$HOST" "$SNAPSHOT" "$BUILD" 'begin' @@MODULE@@ @@STEP@@ || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.checkpoint' @@MODULE@@'.'@@STEP@@'.begin')
	    
	    cd "$BUILD_ROOT"/@@module@@
	    ((@@COMMAND@@) > "$ROOT"/@@module@@.@@step@@.log 2>&1)
	    ERR=$?
	    echo "exit: $ERR" > "$ROOT"/@@module@@.@@step@@.log.meta
	    
	    cd "$ROOT"
	    (upload "$HOST" "$SNAPSHOT" "$BUILD" @@MODULE@@ @@STEP@@ @@module@@.@@step@@.log @@module@@.@@step@@.log.meta || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.upload' @@MODULE@@'.'@@STEP@@)
	    
	    (checkpoint "$HOST" "$SNAPSHOT" "$BUILD" 'end' @@MODULE@@ @@STEP@@ || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.checkpoint' @@MODULE@@'.'@@STEP@@'.end')
	    if [ x"$ERR" = x0 ]; then true; else false; fi
	} @@&&@@
##MODULE-STEP-END##
    }
    
    (checkpoint "$HOST" "$SNAPSHOT" "$BUILD" 'end' @@MODULE@@ || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.checkpoint' @@MODULE@@'.end')
}
##MODULE-END##

(checkpoint "$HOST" "$SNAPSHOT" "$BUILD" 'end' || error "$HOST" "$SNAPSHOT" "$BUILD" 'ready.checkpoint' 'end')
