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
ignore (Sys.command ("touch timestamp_latest_build"));;

(* open Shell;; *)

let call = Shell.call
let to_file = Shell.to_file
let (>&) = Shell.(>&)

let build_date_path =
  let tv = global_start_tv in
    Printf.sprintf "%04i/%02i/%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday
;;

let get_date_time () =
  let tv = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;

Sys.command "test -d da_build_dir || mkdir da_build_dir";;
Sys.command "test -d logs || mkdir logs";;

type build_module =
    Autogen
  | Configure of string list
  | Make of string
  | Time of string
  | Link of (string * string)
  | Unlink of string
  | Patch of string

let loggage x =
  match x with
    Autogen -> `overwrite "autogen_sh"
  | Configure _ -> `overwrite "configure"
  | Make s -> `overwrite ("make_" ^ s)
  | Time _ -> `append "timing"
  | Link _ -> `ignore
  | Unlink _ -> `ignore
  | Patch _ -> `append "patch"

let string_of_mod x =
  match x with
    Autogen -> "autogen_sh"
  | Configure _ -> "configure"
  | Make s -> ("make_" ^ s)
  | Time _ -> "timing"
  | Link _ -> "link"
  | Unlink _ -> "unlink"
  | Patch _ -> "patch"


let base_begin =
  [Time "BEGIN"]
let base_end =
  [Time "END"]

let real_root = Sys.getcwd ()
let mingw32msvc_root = real_root ^ "/mingw32msvc/target";;
let build_dir = Sys.getcwd () ^ "/da_build_dir"
let prefix = build_dir ^ "/target";;

let () = if Array.length Sys.argv = 1 then ignore (Sys.command ("rm -rf " ^ prefix)) in
ignore (Sys.command ("mkdir -p " ^ prefix ^ "/share/aclocal"));;

let autotools_native_src ~configure_args ~patch () =
  base_begin @
  (List.map (fun x -> Patch x) patch)
  @ [Autogen;Configure (["--prefix=" ^ prefix;] @ configure_args);Make "all";Make "check";Make "install"] @ base_end
(* let autotools_native = *)
(*   autotools_native_src ~patch:[] () *)

let prefix_mingw32 = prefix ^ "_mingw32";;

let () = ignore (Sys.command ("rm -rf " ^ prefix_mingw32)) in
ignore (Sys.command ("mkdir -p " ^ prefix_mingw32 ^ "/share/aclocal"));;

let autotools_mingw32_src ~configure_args ~patch () =
  base_begin @
  (List.map (fun x -> Patch x) patch)
  @ [Autogen;Configure (["--prefix=" ^ prefix_mingw32; "--host=i386-mingw32"; "--build=i686-pc-linux-gnu"; "--disable-shared";"--enable-static";] @ configure_args);Make "all";Make "install"] @ base_end
(* let autotools_mingw32 = *)
(*   autotools_mingw32_src ~patch:[] () *)

let autotools ?(pre=[]) ?(post=[]) ?(patch=[]) ?(debug=true) ?(configure=(fun _ -> [])) (system : [`native|`mingw32]) : build_module list =
  let configure_args = if debug then "--enable-debug" :: (configure system) else configure system in
  pre @
  (match system with
    `native ->
      autotools_native_src ~configure_args ~patch ()
  | `mingw32 ->
      autotools_mingw32_src ~configure_args ~patch ()
  ) @ post

let base_logdir = Sys.getcwd () ^ "/logs"
let logdir = Sys.getcwd () ^ "/logs/" ^ build_date_path
let logdir =
  let rec aux n =
    let dir = (logdir ^ "/" ^ (Printf.sprintf "%03i" n)) in
      if not (Sys.file_exists dir)
      then dir
      else if n > 999
      then failwith "unable to find logdir id!"
      else aux (n + 1)
  in aux 1
;;
let logdir_global = logdir;;

(*let logdir = logdir ^ "/logs";;*)
let logdir_platform = logdir ^ "/hosts/demitar-cat"
let logdir = logdir_platform ^ "/builds/001";; (* FIXME *)
let () = ignore (Sys.command ("echo " ^ logdir ^ " >> global.state"));;

List.iter (fun x -> ignore (Sys.command (Printf.sprintf "test -d %s || mkdir -p %s" x x))) [logdir(*; logdir_mingw32*)];;

type vcs_cvs = { vc_repository : string; vc_path : string; vc_branch : string; vc_time : Unix.tm }
type vcs_subversion = { vs_repository : string; vs_path : string; vs_revision : int }
type vcs_git = { vg_repository : string; vg_branch : string; vg_commit : string }
type vcs = CVS of vcs_cvs | Subversion of vcs_subversion | Git of vcs_git
type target = {
  name : string; (* no special chars please *)
  vcs : vcs;
  (* sources reside in source/vcs_type/md5(repository)/cvs_or_svn_path/name *)
  rel_build_dir : string;
  cvs_dir : string;
  cvs_tag : string option;
  environment : (string * string) list;
  modules : build_module list;
  logbase : string;
}
let target ?(cvs_tag=None) ?(modules : [`native|`mingw32] -> build_module list = (autotools ~pre:[] ~post:[] ~patch:[] ~debug:true ~configure:(fun _ -> []))) ?(env=(function _ -> [])) name =
  name, cvs_tag, (fun x -> (("NOCONFIGURE", "yes") :: env x)), modules

let to_build =
  let cvs_root = Sys.getcwd () in
  let gamma =
    (fun (vcs) (name, cvs_tag, env, (modules : [`native|`mingw32] -> build_module list)) ->
       let vcs, source_dir =
	 let md5 str =
	   Cryptokit.transform_string (Cryptokit.Hexa.encode ()) (Cryptokit.hash_string (Cryptokit.Hash.md5 ()) str)
	 in
	 let calc_source_dir type_str repo prefix = Printf.sprintf "%s/source/%s/%s/%s%s" cvs_root type_str (md5 repo) prefix name in
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
		     Sys.chdir cvs_root;
		     Buffer.sub buffer 0 40 (* avoid the newline *)
		 in
		   Git { vg_repository = repository; vg_branch = branch; vg_commit = git_tip_of_branch source_dir branch }, source_dir
	     (* sources reside in source/vcs_type/md5(repository)/cvs_or_svn_path/name *)
       in
       [
	 {
	   name = name;
	   vcs = vcs;
	   rel_build_dir = name;
	   cvs_dir = source_dir;
	   cvs_tag = cvs_tag;
	   environment = ("LC_ALL", "C") :: ("PKG_CONFIG_PATH", prefix ^ "/lib/pkgconfig:/usr/local/lib/pkgconfig") :: ("LD_LIBRARY_PATH", prefix ^ "/lib") :: ("ACLOCAL_FLAGS", "-I " ^ prefix ^ "/share/aclocal" ^ (if Sys.file_exists (*is_directory*) "/usr/local/share/aclocal" then " -I /usr/local/share/aclocal" else "") ^ " -I /usr/share/aclocal") :: env `native;
	   modules = modules `native;
	   logbase = Printf.sprintf "%s/%s.log." logdir name;
	 };
       ]
    )
  in
  let beta m x = gamma (`cvs (":pserver:cvsanon@cvs.worldforge.org:2401/home/cvspsrv/worldforge", ("forge/" ^ m), "HEAD")) x in
  let git m =
    gamma (`git (Printf.sprintf "git://git.worldforge.org/%s.git" m, "master")) (target m)
  in
    (*
	REPOS='cvs%:pserver:cvsanon@cvs.worldforge.org:2401/home/cvspsrv/worldforge%forge'
	for MODULE in ember sear libwfut; do
            REPOS="$REPOS git%git://git.worldforge.org/$MODULE.git%$MODULE"
    *)
    git "libwfut" @
      (List.flatten
	 (List.flatten
            (List.map
	       (fun (m, lst) ->
		  List.map
		    (beta m)
		    lst
	       ) [
		 "libs",
		 [
		   target "skstream";
		   target "varconf";
		   target ~env:(fun _ -> ["PYTHONPATH", cvs_root ^ "/forge/libs/Atlas-Python/"]) "Atlas-C++";
		   target "wfmath";
		   target "mercator";
		   target "sage";
		   target "eris";
		 ];
		 
		 "scratchpad",
		 [
		   target "tree";
		 ];
		 
		 "tools",
		 [
		   target "entityforge";
		 ];
		 
		 "servers",
		 [
		   target "indri";
		   target ~modules:(autotools) "cyphesis-C++";
		   target ~modules:(autotools) "venus";
		 ];
		 
		 "clients",
		 [
		   target "equator";
		   target "apogee";
		   target "silence";
		   target "process";
		 ];
		 
	       ]
            )
	 )
      )
    @ git "sear"
    @ git "ember"
;;

let dump_evals meta x =
  let render (how, n) =
    Printf.fprintf meta " %s,%i" how n
  in
  render
  (match snd x with
    Unix.WEXITED n -> "EXIT", n
  | Unix.WSIGNALED n -> "SIGNAL", n
  | Unix.WSTOPPED n -> "STOP", n)
;;

(* store revisions *)
let tm_to_string tv = Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;
let store_revisions to_build =
  let ch = open_out (logdir_global ^ "/revisions") in
  let pr key value = Printf.fprintf ch "%s %s\n" key value in
    List.iter
      (fun { name = name; vcs = vcs; } ->
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
store_revisions to_build;;
let store_platform () =
  Sys.command ("lsb_release -a > " ^ (logdir_platform ^ "/platform"));
;;
store_platform ();;

let ub = (^) "/usr/bin/";;

let process_module ~cvs_tag ~stdout ~meta ~assignments ~environment m =
  let c x y = 
    Printf.eprintf "RUN: ";
    Shell_sys.iter_env_vars
      ~f:(fun evar eval ->
        Printf.eprintf "%s=\"%s\" " evar eval;
         ) environment;
    Printf.eprintf "%s " x;
    List.iter
      (fun arg ->
        Printf.eprintf "\"%s\" " arg;
      ) y;
    Printf.eprintf "\n";
    Shell.cmd ~assignments ~environment x y
  in
  try
    Shell.call ~stdout
      [
       match m with
	 Autogen ->
	   c "./autogen.sh" []
       | Configure args -> 
	   c "./configure" args
       | Make target ->
	   c (ub "make") [target]
       | Time state ->
	   c "/bin/echo" ["BUILD"; state; "module"; get_date_time ()]
       | Link (source, target) ->
	   c "/bin/ln" ["-s"; source; target];
       | Unlink target ->
	   c "/bin/rm" [target];
       | Patch source ->
	   c (ub "patch") ["-p0"; "--verbose"; "-i"; source]
     ];
    true
  with
    Shell.Subprocess_error res ->
      output_string meta "result:";
      List.iter (dump_evals meta) res;
      List.fold_left
        (fun a b ->
          match b, m with
            Unix.WEXITED 0, _ -> a
          | Unix.WEXITED 1, Patch _ -> a
          | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _), (Make ("check" | "install") | Link _ | Unlink _) -> a
          | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _), (Patch _ | Make _ | Time _ | Configure _ | Autogen)
            -> false)
        true (List.map snd res);
  | Shell_sys.Fatal_error _
  | Unix.Unix_error _ as e ->
      List.iter (fun x -> Printf.fprintf meta "exception: %s\n" x) (Pcre.split ~pat:"\n" (Printexc.to_string e));
      false
;;

List.iter
  (fun t ->
     if (not (Array.length Sys.argv = 2)) || (Sys.argv.(1) = t.name) then begin
    (*     { name string; environment; modules; logbase; } -> *)
    Printf.eprintf "debug: build start %s\n" t.name; flush stderr;

    Sys.chdir build_dir;
    ignore (Sys.command ("rm -rf " ^ t.rel_build_dir));
    ignore (Sys.command ("cp -r " ^ t.cvs_dir ^ " " ^ t.rel_build_dir));
    let cont = ref true in    
    List.iter
      (fun m ->
        if !cont || (match m with Time _ -> true | _ -> false) then begin
	  begin try begin
            Printf.eprintf "debug: module %s begin (%s)\n" (string_of_mod m) t.name; flush stderr;
	    Sys.chdir (build_dir ^ "/" ^ t.rel_build_dir);
	    let environment = (Shell_sys.copy_env (Shell_sys.current_env ())) in
	    List.iter (fun (x, y) -> Shell_sys.set_env_var environment x y) t.environment;
	    let cleanup = ref [] in
            cont := (
	      process_module
		~cvs_tag:t.cvs_tag
		~stdout:
		(match loggage m with
	          `append file ->
	            let fd = Unix.openfile (t.logbase ^ file) [Unix.O_WRONLY;Unix.O_APPEND;Unix.O_CREAT] 0o644 in
	            cleanup := (fun () -> Unix.close fd) :: !cleanup;
	            Shell.to_fd fd
		| `overwrite file ->
	            to_file ~append:false
		      (t.logbase ^ file)
		| `ignore ->
	            Shell.to_dev_null
		)
		~meta:
		(match loggage m with
	          `append file ->
                    let meta = open_out_gen [Open_wronly;Open_append;Open_creat;Open_text] 0o644 (t.logbase ^ file ^ ".meta") in
	            cleanup := (fun () -> close_out meta) :: !cleanup;
                    meta
		| `overwrite file ->
                  let meta = open_out_gen [Open_wronly;Open_trunc;Open_creat;Open_text] 0o644 (t.logbase ^ file ^ ".meta") in
	          cleanup := (fun () -> close_out meta) :: !cleanup;
                  meta
		| `ignore ->
                    let meta = open_out "/dev/null" in
	            cleanup := (fun () -> close_out meta) :: !cleanup;
                    meta
		)
		~assignments:[Shell.stderr >& Shell.stdout] ~environment m);
	    List.iter (fun f -> f ()) !cleanup;
	  end with
	    ex -> 
	      prerr_endline ("Unable to perform build step due to unexpected event:\n" ^ (Printexc.to_string ex));
	  end;
          Printf.eprintf "debug: module %s end (%s)\n" (string_of_mod m) t.name; flush stderr;
        end;
      ) t.modules;
    Printf.eprintf "debug: build end %s\n" t.name; flush stderr;
    (* generate logs *)
    Sys.chdir real_root;
    ignore (Sys.command "./trurl_render_log ../public_html/");
     end
  ) to_build
