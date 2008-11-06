<?php
  /*
    Copyright 2008 Anders Petersson

    This file is part of Trurl.

    Trurl is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Trurl is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Trurl.  If not, see <http://www.gnu.org/licenses/>.
  */
?>
<?php

function fail($msg) {
  header('HTTP/1.0 500 Internal Server Error');
  /* FIXME, log */
  echo 'fail: ' . $msg . "\n";
  die();
}

function highest_dir($path) {
  $dir = dir($path);
  $highest = false;
  while (false !== ($entry = $dir->read())) {
     if ($entry !== "." && $entry !== "..")
       $highest = max($entry, $highest);
  }
  $dir->close();
  return $highest;
}

function list_dir($path) {
  $dir = dir($path);
  $entries = array();
  while (false !== ($entry = $dir->read())) {
     if ($entry !== "." && $entry !== "..")
       $entries[] = $entry;
  }
  $dir->close();
  return $entries;
}

function safe_shared($str) {
  if ($str == '') {
    fail('safe_shared: empty string');
  }
  return $str;
}

function safe_soft($str) {
  $str = preg_replace('%[^-A-Za-z_0-9+]%', '_', $str);
  return safe_shared($str);
}

function safe_hard($str) {
  $str = strtolower($str);
  $str = preg_replace('%[^-a-z_0-9]%', '_', $str);
  return safe_shared($str);
}

?>
