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

date_default_timezone_set('UTC');
umask(octdec('022'));

require_once('shared.php');

function bootstrap() {
  global $http_root, $root;
  /*  $year = highest_dir($log_root);
  $month = highest_dir($log_root . '/' . $year);
  $day = highest_dir($log_root . '/' . $year . '/' . $month);
  $snap = highest_dir($log_root . '/' . $year . '/' . $month . '/' . $day);*/
  //  $bootstrap_template = file_get_contents($log_root . '/' . $year . '/' . $month . '/' . $day . '/' . $snap . "/instructions/" . safe_hard($_POST['archetype']) . "/bootstrap.sh") or die('Unable to read template.');
  $template = @file_get_contents($root . "/bootstrap." . safe_hard($_POST['archetype']) . ".sh") or fail('Unable to read template.');
  $template = preg_replace("/@@HTTP_ROOT@@/", "'" . $http_root . "'", $template);
  $template = preg_replace("/@@HOST@@/", "'" . safe_hard($_POST['host']) . "'", $template);
  echo $template;
}

function ready() {
  global $http_root, $http_source_root, $log_root;
  //  preg_match("/^(\d+).(\d+).(\d+)-(\d+)$/");
  $year = highest_dir($log_root);
  $month = highest_dir($log_root . '/' . $year);
  $day = highest_dir($log_root . '/' . $year . '/' . $month);
  $snap = highest_dir($log_root . '/' . $year . '/' . $month . '/' . $day);
  $snapshot_dir = $log_root . '/' . $year . '/' . $month . '/' . $day . '/' . $snap;

  // find last build
  // if none -> build
  // check if it's finished?
  // compare platform and system to the uploaded
  // if changed -> build
  // else up to date

  $builds_dir = $snapshot_dir . '/hosts/' . safe_hard($_POST['host']) . '/builds';
  $rebuild = false;
  if (file_exists($builds_dir)) {
    $last_build = highest_dir($builds_dir);
    $platform_file = $builds_dir . '/' . $last_build . '/platform';
    $system_file = $builds_dir . '/' . $last_build . '/system.tar';
    if (file_exists($platform_file)) {
      $last_platform = @file_get_contents($platform_file) or fail('Unable to read previous platform file.');
      if ($last_platform != $_POST['platform']) {
	$rebuild = true;
      }
    } else {
      $rebuild = true;
    }
    if (file_exists($system_file)) {
      system('tar xOf "' . $system_file . '" > "' . $system_file . '.extracted"');
      $last_system = @file_get_contents($system_file . '.extracted') or fail('Unable to read previous system file.');
      $system_file_incoming = $system_file . '.incoming';
      file_put_contents($system_file_incoming, $_POST['system']);
      system('tar xOf "' . $system_file_incoming . '" > "' . $system_file_incoming . '.extracted"');
      $new_system = file_get_contents($system_file_incoming . '.extracted');
      if ($last_system != $new_system) {
	$rebuild = true;
      }
    } else {
      if (isset($_POST['system'])) {
	$rebuild = true;
      }
    }
  } else {
    $rebuild = true;
  }

  if ($rebuild) {
    $template = @file_get_contents($snapshot_dir . "/instructions/ready." . safe_hard($_POST['archetype']) . ".sh") or fail('Unable to read template.');
    $template = preg_replace("/@@HTTP_ROOT@@/", "'" . $http_root . "'", $template);
    $template = preg_replace("/@@HTTP_SOURCE_ROOT@@/", "'" . $http_source_root . "'", $template);
    $template = preg_replace("/@@HOST@@/", "'" . safe_hard($_POST['host']) . "'", $template);

    if (file_exists($builds_dir)) {
      $last_build = highest_dir($builds_dir);
      if ($last_build == false) {
	$build = '001';
      } else {
	$build = sprintf('%03d', intval($last_build) + 1);
      }
    } else {
      $build = '001';
    }
    $build_dir = $snapshot_dir . '/hosts/' . safe_hard($_POST['host']) . '/builds/' . $build;
    mkdir($build_dir, 0777, true);

    $fd = @fopen($build_dir . "/platform", 'x') or fail('Unable to write platform information (exists or otherwise).');
    fwrite($fd, $_POST['platform']);
    fclose($fd);

    if (isset($_POST['system'])) {
      $fd = @fopen($build_dir . "/system.tar", 'x') or fail('Unable to write system information (exists or otherwise).');
      fwrite($fd, $_POST['system']);
      fclose($fd);
    }

    $template = preg_replace("/@@BUILD@@/", "'" . $build . "'", $template);
    //@@BUILD@@
    // /hosts/$host/builds/%03i
    echo $template;
  } else {
    echo 'echo "Build up to date."';
  }
}

function checkpoint() {
  global $log_root;
  if (preg_match('/^(\d+).(\d+).(\d+)-(\d+)$/', $_POST['snapshot'], $m)) {
    $year = $m[1];
    $month = $m[2];
    $day = $m[3];
    $snap = $m[4];
    $snapshot_dir = $log_root . '/' . $year . '/' . $month . '/' . $day . '/' . $snap;
    if (!is_dir($snapshot_dir)) {
      fail('No such snapshot: ' . $_POST['snapshot']);
    }
    $host_dir = $snapshot_dir . '/hosts/' . safe_hard($_POST['host']);
    if (!is_dir($host_dir)) {
      fail('No such host:' . $_POST['host']);
    }
    $build_dir = $host_dir . '/builds/' . safe_hard($_POST['build']);
    if (!is_dir($build_dir)) {
      fail('No such build:' . $_POST['build']);
    }
    $fd = fopen($build_dir . '/checkpoints', 'a') or fail('Unable to open checkpoints file.');
    $checkpoint = date('c') . ' ' . safe_hard($_POST['edge']);
    if (isset($_POST['module'])) {
      $checkpoint .= ' ' . safe_soft($_POST['module']);
    }
    if (isset($_POST['step'])) {
      $checkpoint .= ' ' . safe_soft($_POST['step']);
    }
    fwrite($fd, $checkpoint . "\n");
    fclose($fd);
    /*    ignore_user_abort(true);
    header("Connection: close");
    header("Content-Length: 0");
    flush();*/
    if ($_POST['edge'] == 'end' && !isset($_POST['step'])) {
      touch("/home/trurl/work/force/render");
      /*system("/home/trurl/work/fork_update_logs.sh");*/
    }
    return;
  }
  fail('Unable to parse snapshot.');
}

function upload() {
  global $log_root;
  if (preg_match('/^(\d+).(\d+).(\d+)-(\d+)$/', $_POST['snapshot'], $m)) {
    $year = $m[1];
    $month = $m[2];
    $day = $m[3];
    $snap = $m[4];
    $snapshot_dir = $log_root . '/' . $year . '/' . $month . '/' . $day . '/' . $snap;
    if (!is_dir($snapshot_dir)) {
      fail('No such snapshot: ' . $_POST['snapshot']);
    }
    $host_dir = $snapshot_dir . '/hosts/' . safe_hard($_POST['host']);
    if (!is_dir($host_dir)) {
      fail('No such host:' . $_POST['host']);
    }
    $build_dir = $host_dir . '/builds/' . safe_hard($_POST['build']);
    if (!is_dir($build_dir)) {
      fail('No such build:' . $_POST['build']);
    }
    $modules_dir = $build_dir . '/modules';
    if (!is_dir($modules_dir)) {
      mkdir($modules_dir, 0777, true);
    }
    $log_file = $modules_dir . '/' . safe_soft($_POST['module']) . '.log.' . safe_soft($_POST['step']);
    $meta_file = $modules_dir . '/' . safe_soft($_POST['module']) . '.log.' . safe_soft($_POST['step']) . '.meta';
    $fd = @fopen($log_file, 'x') or fail('Unable to open target logfile (exists or otherwise).');
    fwrite($fd, $_POST['log']);
    fclose($fd);
    $fd = @fopen($meta_file, 'x') or fail('Unable to open target metafile (exists or otherwise).');
    fwrite($fd, $_POST['meta']);
    fclose($fd);
    return;
  }
  fail('Unable to parse snapshot.');
}

function error() {
  fail('error.unimplemented');
}

if (count($_POST) == 0) {
  $_POST = $_GET;
}
if($_GET['action'] == "bootstrap") {
  bootstrap();
 } else {
  switch ($_POST['action']) {
  case "bootstrap":
    bootstrap();
    break;
  case "ready":
    ready();
    break;
  case "checkpoint":
    checkpoint();
    break;
  case "upload":
    upload();
    break;
  case "error":
    error();
    break;
  default:
    if (count($_GET) == 0 && count($_POST) == 0) {
      echo '<?xml version="1.0" encoding="utf-8" ?>' . "\n";
      echo
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>Autobuilder Client</title>
</head>
<body>
";
      echo "<dl>\n";
      echo "<dt>Posix</dt>\n";
      echo "<dd>";
      echo htmlentities('curl -s -S -f "' . $http_root . '?action=bootstrap&archetype=posix&host=" -o bootstrap.sh && bash bootstrap.sh');
      echo "</dd>\n";
      echo "</dl>\n";

      echo "<dl>\n";
      echo "<dt>Static curl</dt>\n";
      echo "<dd><a href=\"http://www.magicermine.com/demos/curl/curl/curl.html\">http://www.magicermine.com/demos/curl/curl/curl.html</a></dd>\n";
      echo "</dl>\n";

      echo "</body>\n</html>\n";
    } else {
      fail('action does not exist: ' . $_POST['action']);
    }
    break;
  }
 }

?>
