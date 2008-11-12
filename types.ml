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

type snapshot = (int * int * int * int) (* year, month, day, count *)

type result = Error | Dependency_error | Warning | Information | Unknown
type section = {
  s_result : result;
  s_lines : (int * string * result * (int * string) option) list; (* (line_no, line, result, (n_regular_expression_matched, matching_regular_expression) option) *)
}
type logfile = {
  f_step : string;
  f_filename : string;
  f_result : result;
  f_sections : section list;
}

type tr_build = {
  tb_build : int;
  tb_host : string;
  tb_start : Unix.tm;
  tb_time : float;
  tb_result : result;
  tb_logs : logfile list;
}
type tr_platform = {
  tp_snapshot : snapshot;
  tp_module : string;
  tp_platform : string; (* ubuntu-8.10 *)
  tp_result : result;
  tp_builds : tr_build list;
}

type vcs_cvs = { vc_repository : string; vc_path : string; vc_branch : string; vc_time : Unix.tm }
type vcs_subversion = { vs_repository : string; vs_path : string; vs_revision : int }
type vcs_git = { vg_repository : string; vg_branch : string; vg_commit : string }
type vcs = CVS of vcs_cvs | Subversion of vcs_subversion | Git of vcs_git
type tr_module_building = {
  tmb_platform : string;
  tmb_host : string;
}
type tr_module_scheduled = {
  tms_platform : string;
  tms_host : string;
}
type tr_module = {
  tm_snapshot : snapshot;
  tm_module : string; (* constructed name: module/branch eg: Atlas-C++/0.5 Atlas-C++/HEAD; module = basename path or /($module).git *)
  tm_revision : vcs;
  tm_result : result;
  tm_platforms : tr_platform list;
  tm_building : tr_module_building list;
  tm_scheduled : tr_module_scheduled list;
}
type tr_snapshot = {
  ts_snapshot : snapshot;
  ts_result : result;
  ts_modules : tr_module list;
}
type tr_tip = tr_module list
type tr_logs = {
  tl_snapshots : tr_snapshot list;
  tl_tip : tr_tip;
}

let safe_string_of_result r =
  match r with
  | Error -> "error"
  | Dependency_error -> "dependency_error"
  | Warning -> "warning"
  | Information -> "information"
  | Unknown -> "unknown"

let snapshot_safe_string (year, month, day, build) =
  Printf.sprintf "%04i.%02i.%02i-%03i" year month day build
;;
