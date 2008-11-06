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

require_once('shared.php');

function trurl_last($last) {
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
  echo "<img src=\"static/images/building-16x16.png\" alt=\"\" />";
  echo "Force rebuild</button>";
  echo "</form>";
  echo "<form action=\"action.php\" method=\"post\">";
  echo "<button class=\"button\" type=\"submit\" name=\"force_poll\" value=\"Force poll\">";
  echo "<img src=\"static/images/updating-16x16.png\" alt=\"\" />";
  echo "Force poll</button>";
  echo "</form>";
}

function trurl_string_begins_with($haystack, $needle)
{
    return (strncmp($haystack, $needle, strlen($needle)) == 0);
}

function trurl_global_state() {
  if (file_exists("/home/trurl/work/force/build") || file_exists("/home/trurl/work/force/update")) {
    $state = 'update';    
  } else {
    $state = 'idle';
  }
  
  /*
   check three last snapshots
   check all hosts not already found
   check latest build's checkpoints
   if doesn't end in end -> host is building
   else -> finished
  */
  
  $root = "/home/trurl/work/logs";
  //  echo $root . ' ';
  $root_year = $root . '/' . highest_dir($root);
  //  echo $root_year . ' ';
  $root_month = $root_year . '/' . highest_dir($root_year);
  //  echo $root_month . ' ';
  $root_day = $root_month . '/' . highest_dir($root_month);
  //  echo $root_day . ' ';
  $root_snapshot = $root_day . '/' . highest_dir($root_day);
  //  echo $root_snapshot . ' ';
  $root_hosts = $root_snapshot . '/hosts';
  //  echo $root_hosts . ' ';
  $hosts_seen = 0;
  $hosts_finished = 0;
  $latest_build = 0;
  if (is_dir($root_hosts)) {
    $hosts = list_dir($root_hosts);
    foreach($hosts as $host) {
      //      echo $host . ' ';
      $root_builds = $root_hosts . '/' . $host . '/builds';
      if (is_dir($root_builds)) {
	$build = highest_dir($root_builds);
	$checkpoints_path = $root_builds . '/' . $build . '/checkpoints';
	if (is_file($checkpoints_path)) {
	  $latest_build = max($latest_build, filemtime($checkpoints_path));
	  $arr = file($checkpoints_path);
	  end($arr);
	  $res = preg_match('/^\d+-\d+-\d+T\d+:\d+:\d+(\+00:00|Z) end$/', current($arr));
	  $hosts_seen++;
	  if ($res) {
	    $hosts_finished++;
	  }//echo $res . ' ' . current($arr);
	}
      }
      //    print_r();
    }
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

  if ($hosts_finished < $hosts_seen) {
    $state = 'build';
  }

  echo "<p class=\"global state\">";
  switch ($state) {
    case 'build':
      echo "<img src=\"static/images/building.png\" alt=\"\" /> Building</p>\n";
      echo "<p class=\"global eta\">"; echo ($hosts_seen - $hosts_finished) . "/$hosts_seen hosts building.</p>\n";
      // XXX abort build (combine with force command?)
      /*      echo "<p class=\"global buttons\">";
  echo "<form action=\"action.php\" method=\"post\">";
  echo "<button class=\"button\" type=\"submit\" name=\"force_build\" value=\"Force rebuild\">";
  echo "<img src=\"static/images/building-16x16.png\" alt=\"\" />";
  echo "Queue forced rebuild</button>";
  echo "</form>";
  echo "<form action=\"action.php\" method=\"post\">";
  echo "<button class=\"button\" type=\"submit\" name=\"force_poll\" value=\"Force poll\">";
  echo "<img src=\"static/images/updating-16x16.png\" alt=\"\" />";
  echo "Queue forced poll</button>";
  echo "</form>";
  echo "</p>\n";*/
      break;
    case 'update':
      echo "<img src=\"static/images/updating.png\" alt=\"\" /> Updating</p>\n";
      echo "<p class=\"global eta\">"; trurl_last($latest_build); /*echo "&nbsp;";*/ echo "</p>\n";
      /*      echo "<p class=\"global buttons\">"; trurl_finished_buttons(); echo "</p>\n";*/
      break;
    default:
      echo "<img src=\"static/images/internal_error.png\" alt=\"\" />";
    case 'idle':
      echo "<img src=\"static/images/finished.png\" alt=\"\" /> Finished</p>\n";
      echo "<p class=\"global eta\">"; trurl_last($latest_build); echo "</p>\n";
      /*      echo "<p class=\"global buttons\">"; trurl_finished_buttons(); echo "</p>\n";*/
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
