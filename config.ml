(*
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
*)

let enable_debug_features = false;;
let platform_image_version = "-16x16";;
let platform_image_version_mistake = "" (* platform_image_version *);;
let ansi_error = if enable_debug_features then "\027[01;31mERROR:\027[0m " else "ERROR: ";;
let ansi_debug = if enable_debug_features then "\027[01;32mDEBUG:\027[0m " else "DEBUG: ";;

let output_dirty = ref false
let error_endline str =
  output_dirty := true;
  prerr_endline (ansi_error ^ str)
let debug_endline str =
  if enable_debug_features then begin
    output_dirty := true;
    prerr_endline (ansi_debug ^ str)
  end else
    ();;

let report_if_dirty () =
  if enable_debug_features then
    prerr_endline ("\027[01;30m" ^ Sys.argv.(0) ^ " FINISHED\027[0m")
  else
    ();;
at_exit report_if_dirty;;

let target_dir = Sys.argv.(1)
let generate_n_logs =
  try
    int_of_string Sys.argv.(2)
  with _ ->
    6

let html_root =
  try
    if Sys.argv.(2) = "--testbed" then "/trurl/testbed/" else "/trurl/"
  with _ -> "/trurl/";;

let rules_filename = "trurl_log_rules.xml"
let stat_self = Unix.stat Sys.argv.(0)
let stat_rules = Unix.stat rules_filename

let snapshot_hard_limit = 10 (*FIXME, use lazy*);;
