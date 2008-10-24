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

function trurl_eta() {
  $eta = 0;
  /*  file_mtime("cvs.forge.update")
    file_mtime("git.ember.update");
      Printf.fprintf ch "<dt>Build last started</dt>\n";
      Printf.fprintf ch "<dd>%s</dd>\n" (render_stat_mtime "timestamp_latest_build");*/
  $eta = 60 - date('i');
  echo "$eta minutes until next poll.";
}

function trurl_last() {
  $eta = 0;
  /*  file_mtime("cvs.forge.update")
    file_mtime("git.ember.update");
      Printf.fprintf ch "<dt>Build last started</dt>\n";
      Printf.fprintf ch "<dd>%s</dd>\n" (render_stat_mtime "timestamp_latest_build");*/
  $last = filemtime("/home/trurl/work/timestamp_latest_build");
  $delta = (time() - $last);
  $days = $delta / (24 * 60 * 60);
  if (floor($days)) {
    echo round($days, $days > 2 ? 0 : 1) . " days";
  } else {
    $hours = $delta / (60 * 60);
    if (floor($hours)) {
      echo round($hours, $hours > 2 ? 0 : 1) . " hours";
    } else {
      $minutes = $delta / (60);
      echo round($minutes, $minutes > 2 ? 0 : 1) . " minutes";
    }
  }
  echo " since last build.";
}

function trurl_finished_buttons() {
  echo "<form action=\"action.php\" method=\"post\">";
  echo "<button class=\"button\" type=\"submit\" name=\"force_build\" value=\"Force rebuild\">";
  echo "<img src=\"static/images/building-16x16.png\" />";
  echo "Force rebuild</button>";
  echo "</form>";
  echo "<form action=\"action.php\" method=\"post\">";
  echo "<button class=\"button\" type=\"submit\" name=\"force_poll\" value=\"Force poll\">";
  echo "<img src=\"static/images/updating-16x16.png\" />";
  echo "Force poll</button>";
  echo "</form>";
}

function trurl_string_begins_with($haystack, $needle)
{
    return (strncmp($haystack, $needle, strlen($needle)) == 0);
}

function trurl_global_state($building) {
  $building = $building;
  if ($building) {
    $state = 'build';
  } else if (file_exists("/home/trurl/work/force/build") || file_exists("/home/trurl/work/force/update")) {
      $state = 'update';    
  } else {
    $state = 'idle';
  }
  $statefile = "/home/trurl/work/global.state";
  if (file_exists($statefile)) {
    $statestring = file_get_contents($statefile);
    if (trurl_string_begins_with($statestring, "build\n")) {
      $state = 'build';
    } else if (trurl_string_begins_with($statestring, "update\n")) {
      $state = 'update';
    }
  }
  echo "<p class=\"global state\">";
  switch ($state) {
    case 'build':
      echo "<img src=\"static/images/building.png\" /> Building</p>\n";
      echo "<p class=\"global eta\">"; /* trurl_eta(); */ echo "0/1 platforms finished.</p>\n";
      // XXX abort build (combine with force command?)
      echo "<p class=\"global buttons\">";
  echo "<form action=\"action.php\" method=\"post\">";
  echo "<button class=\"button\" type=\"submit\" name=\"force_build\" value=\"Force rebuild\">";
  echo "<img src=\"static/images/building-16x16.png\" />";
  echo "Queue forced rebuild</button>";
  echo "</form>";
  echo "<form action=\"action.php\" method=\"post\">";
  echo "<button class=\"button\" type=\"submit\" name=\"force_poll\" value=\"Force poll\">";
  echo "<img src=\"static/images/updating-16x16.png\" />";
  echo "Queue forced poll</button>";
  echo "</form>";
      echo "</p>\n";
      break;
    case 'update':
      echo "<img src=\"static/images/updating.png\" /> Updating</p>\n";
      echo "<p class=\"global eta\">"; /* trurl_eta(); */ trurl_last(); /*echo "&nbsp;";*/ echo "</p>\n";
      echo "<p class=\"global buttons\">"; trurl_finished_buttons(); echo "</p>\n";
      break;
    default:
      echo "<img src=\"static/images/internal_error.png\" />";
    case 'idle':
      echo "<img src=\"static/images/finished.png\" /> Finished</p>\n";
      echo "<p class=\"global eta\">"; trurl_last(); echo "</p>\n";
      echo "<p class=\"global buttons\">"; trurl_finished_buttons(); echo "</p>\n";
      break;
  }
  echo "<div class=\"clear\"></div>\n";
}

function trurl_action() {
  if (isset($_POST['force_build'])) {
    touch("/home/trurl/work/force/build");
  }
  if (isset($_POST['force_poll'])) {
    touch("/home/trurl/work/force/update");
  }
  header('Location: /trurl/');
}
?>
