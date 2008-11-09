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

let global_start_tv = Unix.gmtime (Unix.time ());;
let global_root = Sys.getcwd ();;
let global_logs = global_root ^ "/logs";;

let md5 str =
  Cryptokit.transform_string (Cryptokit.Hexa.encode ()) (Cryptokit.hash_string (Cryptokit.Hash.md5 ()) str)
;;

let make_snapshot_dir () =
  let base_logdir = global_logs in
  let (build_date_path, (y, m, d)) =
    let tv = global_start_tv in
    let (y, m, d) = (tv.Unix.tm_year + 1900), (tv.Unix.tm_mon + 1), tv.Unix.tm_mday in
      (Printf.sprintf "%04i/%02i/%02i" y m d, (y, m, d))
  in
  let logdir = base_logdir ^ "/" ^ build_date_path in
  let (logdir, n) =
    let rec aux n =
      let dir = (logdir ^ "/" ^ (Printf.sprintf "%03i" n)) in
	if not (Sys.file_exists dir)
	then (ignore (Sys.command (Printf.sprintf "mkdir -p '%s'" dir));
	      (dir, n))
	else if n > 999
	then failwith "unable to find logdir id!"
	else aux (n + 1)
    in aux 1
  in (logdir, (y, m, d, n))
;;

let snapshot_safe_string (year, month, day, build) =
  Printf.sprintf "%04i.%02i.%02i-%03i" year month day build
;;

type vcs_cvs = { vc_repository : string; vc_path : string; vc_branch : string; vc_time : Unix.tm }
type vcs_subversion = { vs_repository : string; vs_path : string; vs_revision : int }
type vcs_git = { vg_repository : string; vg_branch : string; vg_commit : string }
type vcs = CVS of vcs_cvs | Subversion of vcs_subversion | Git of vcs_git

let get_vcs_tip name vcs =
  let calc_source_dir type_str repo prefix = Printf.sprintf "%s/source/%s/%s/%s%s" global_root type_str (md5 repo) prefix name in
    match vcs with
	`cvs (repository, path_prefix, branch) ->
	  CVS { vc_repository = repository; vc_path = path_prefix; vc_branch = branch; vc_time = global_start_tv; }, calc_source_dir "cvs" repository (if path_prefix = "" then "" else path_prefix ^ "/")
      | `git (repository, branch) ->
	  let source_dir = calc_source_dir "git" repository "" in
	  let git_tip_of_branch dir branch =
	    let buffer = Buffer.create 41 in
	    let stdout = Shell.to_buffer buffer in
	      Sys.chdir dir;
	      Shell.call ~stdout [Shell.command ~arguments:[|"-n";"1";branch|] (Shell_sys.lookup_executable "git-rev-list")];
	      Sys.chdir global_root;
	      Buffer.sub buffer 0 40 (* avoid the newline *)
	  in
	    Git { vg_repository = repository; vg_branch = branch; vg_commit = git_tip_of_branch source_dir branch }, source_dir
;;

type b_step = { s_step : string; s_command : string; s_environment : (string * string) list; s_required : bool; }
type b_module = { m_module : string; m_steps : b_step list; m_revision : vcs; m_source_dir : string; }

let get_build_list () =
  let step ?(environment=[]) ?(required=true) step command =
    let base_environment = [("LC_ALL", "C"); ("PKG_CONFIG_PATH", "$TARGET/lib/pkgconfig:/usr/local/lib/pkgconfig"); ("LD_LIBRARY_PATH", "$TARGET/lib")] in
      { s_step = step; s_command = command; s_environment = environment @ base_environment; s_required = required; }
  in
  let automake =
    [
      step ~environment:[("NOCONFIGURE", "yes"); ("ACLOCAL_FLAGS", "-I $TARGET/share/aclocal" (*^ (if Sys.file_exists (*is_directory*) "/usr/local/share/aclocal" then " -I /usr/local/share/aclocal" else "")*) ^ " -I /usr/share/aclocal")] ~required:false "autogen_sh" "./autogen.sh";
      step "configure" "./configure --prefix=\"$TARGET\"";
      step "make_all" "make all";
      step ~required:false "make_check" "make check";
      step ~required:false "make_install" "make install";
    ]
  in
  let module_ vcs m =
    let vcs, source_dir = get_vcs_tip m vcs in
      
      { m_module = m; m_revision = vcs; m_source_dir = source_dir; m_steps = automake; }
  in
  let cvs path m =
    let vcs = (`cvs (":pserver:cvsanon@cvs.worldforge.org:2401/home/cvspsrv/worldforge", ("forge/" ^ path), "HEAD")) in
      module_ vcs m
  in
  let git m =
    let vcs = (`git (Printf.sprintf "git://git.worldforge.org/%s.git" m, "master")) in
      module_ vcs m
  in
    [
      git "libwfut";
      cvs "libs" "skstream";
      cvs "libs" "varconf";
      cvs "libs" "Atlas-C++";
      (*     target ~env:(fun _ -> ["PYTHONPATH", cvs_root ^ "/forge/libs/Atlas-Python/"]) "Atlas-C++";*)
      cvs "libs" "wfmath";
      cvs "libs" "mercator";
      cvs "libs" "sage";
      git "eris";
      (*cvs "scratchpad" "tree";*)
      cvs "tools" "entityforge";
      cvs "servers" "indri";
      cvs "servers" "cyphesis-C++";
      cvs "servers" "venus";
      cvs "clients" "equator";
      cvs "clients" "apogee";
      cvs "clients" "silence";
      cvs "clients" "process";
      git "sear";
      git "ember";
    ]
;;

(* store revisions *)
let tm_to_string tv = Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;
let store_revisions logdir_global to_build =
  let ch = open_out (logdir_global ^ "/revisions") in
  let pr key value = Printf.fprintf ch "%s %s\n" key value in
    List.iter
      (fun { m_module = name; m_revision = vcs; } ->
	 pr "name" name;
	 begin match vcs with
	     CVS { vc_repository = vc_repository; vc_path = vc_path; vc_branch = vc_branch; vc_time = vc_time; } ->
	       pr "vcs" "cvs";
	       pr "cvs.repository" vc_repository;
	       pr "cvs.path" vc_path;
	       pr "cvs.branch" vc_branch;
	       pr "cvs.time" (tm_to_string vc_time);
	   | Subversion (*{ vs_repository = vs_repository; vs_path = vs_path; vs_revision = vs_revision; }*) _ (* FIXME *) ->
	       pr "vcs" "svn-fixme"
	   | Git { vg_repository = vg_repository; vg_branch = vg_branch; vg_commit = vg_commit; } ->
	       pr "vcs" "git";
	       pr "git.repository" vg_repository;
	       pr "git.branch" vg_branch;
	       pr "git.commit" vg_commit;
	 end;
	 Printf.fprintf ch "\n"
    ) to_build;
    close_out ch
;;
