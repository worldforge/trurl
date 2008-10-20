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

type result = (*Perfect |*) Error | Dependency_error | Warning | Information | Unknown
type special = Timing of float
type logfile = {
  f_step : string;
  f_filename : string;
  f_result : result;
  f_special : special option;
  f_lines : (int * string * result) list;
}
type log = {
  l_module : string;
  l_result : result;
  l_logs : logfile list;
}
type logs = log list

type tr_build = {
  tb_host : string;
  tb_time : float;
  tb_start : Unix.tm;
  tb_result : result;
  tb_logs : logfile list;
(*  tb_system : string; system hash to detect the need for rebuild and if that may be responsible for failed/fixed builds *)
}
type tr_platform = {
  tp_platform : string; (* ubuntu-8.10 *)
  tp_result : result;
(*  tp_time : float;
  tp_logs : logfile list;*)
  tp_builds : tr_build list;
}

type vcs_cvs = { vc_repository : string; vc_path : string; vc_branch : string; vc_time : Unix.tm }
type vcs_subversion = { vs_repository : string; vs_path : string; vc_revision : int }
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
type tr_module = { (*"Atlas-C++/0.5" Atlas-C++/HEAD *)
  tm_module : string; (* constructed name: module/branch *)
(*  tl_result : result;*)
  tm_revision : vcs; (* repository (incl path), branch, revision *)
(*  tm_branch : string;*)
(*  tr_revision : vcs_revision;*)
  tm_result : result;
  tm_platforms : tr_platform list;
(*  tl_revisions : tr_revision list;*)
(*  tl_platforms : tr_platform list;*)
(*  tl_time : float option;*)
(*  tl_logfile : logfile;*)
}
type tr_snapshot = {
  ts_snapshot : (int * int * int * int); (* year, month, day, count *)
  ts_result : result;
  ts_modules : tr_module list;
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

