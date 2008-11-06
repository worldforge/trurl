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
type special = Timing of (Unix.tm * float)
type section = {
  s_result : result;
  s_lines : (int * string * result * (int * string) option) list; (* (line_no, line, result, (n_regular_expression_matched, matching_regular_expression) option) *)
}
type logfile = {
  f_step : string;
  f_filename : string;
  f_result : result;
  f_special : special option;
  f_sections : section list;
}
(*type log = {
  l_snapshot : snapshot;
  l_module : string;
  l_result : result;
  l_logs : logfile list;
}
type logs = log list*)

type tr_build = {
  tb_build : int;
  tb_host : string;
  tb_start : Unix.tm;
  tb_time : float;
  tb_result : result;
  tb_logs : logfile list;
(*  tb_system : string; system hash to detect the need for rebuild and if that may be responsible for failed/fixed builds *)
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
(*type vcs_repository = CVS of (string * string) (* second is path inside repository *) | Subversion of (string * string) | Git of string
type vcs_revision = CVS' of Unix.tm (* Won't even try to track cvs revisions *) | Subversion' of int | Git' of string (* git commit object *)*)
(*type tr_revision = {
(*  tr_module : tr_module;*)
(*  tb_result : result;
  tb_repository : string;
  tb_vcs : vcs * int
  tb_id : (int * int * int * int); (* year, month, day, build *)*)
}*)
type tr_module_building = {
  (*  tmb_module : string;*)
  tmb_platform : string;
  tmb_host : string;
}
type tr_module_scheduled = {
(*  tms_module : string;*)
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
(*  ts_building : tr_snapshot_building list;
  ts_scheduled : tr_snapshot_scheduled list;*)
}
type tr_logs = tr_snapshot list

let safe_string_of_result r =
  match r with
    (*Perfect -> "perfect"*)
  | Error -> "error"
  | Dependency_error -> "dependency_error"
  | Warning -> "warning"
  | Information -> "information"
  | Unknown -> "unknown"

