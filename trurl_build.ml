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

ignore (Sys.command ("touch timestamp_latest_build"));;

(* open Shell;; *)

let call = Shell.call
let to_file = Shell.to_file
let (>&) = Shell.(>&)

let build_date_path =
  let tv = Unix.gmtime (Unix.time ()) in
(*  Printf.sprintf "%04i-%02i-%02i_%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour + 1)*)
  Printf.sprintf "%04i/%02i/%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday
;;

let get_date_time () =
  let tv = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour + 1) (tv.Unix.tm_min + 1) (tv.Unix.tm_sec + 1)
;;

Sys.command "test -d da_build_dir || mkdir da_build_dir";;
Sys.command "test -d build_logs || mkdir build_logs";;

type build_module =
    Autogen
  | Configure of string list
  | Make of string
  | Time of string
  | Cvs_diff
  | Link of (string * string)
  | Unlink of string
  | Patch of string

let loggage x =
  match x with
    Autogen -> `overwrite "autogen_sh"
  | Configure _ -> `overwrite "configure"
  | Make s -> `overwrite ("make_" ^ s)
  | Time _ -> `append "timing"
  | Cvs_diff -> `overwrite "diff"
  | Link _ -> `ignore
  | Unlink _ -> `ignore
  | Patch _ -> `append "patch"

let string_of_mod x =
  match x with
    Autogen -> "autogen_sh"
  | Configure _ -> "configure"
  | Make s -> ("make_" ^ s)
  | Time _ -> "timing"
  | Cvs_diff -> "diff"
  | Link _ -> "link"
  | Unlink _ -> "unlink"
  | Patch _ -> "patch"


let base_begin =
  [Time "BEGIN";Cvs_diff]
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

let logdir = Sys.getcwd () ^ "/build_logs/" ^ build_date_path
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
let () = ignore (Sys.command ("echo " ^ logdir ^ " >> global.state"));;
let () = ignore (Sys.command ("rm -rf " ^ logdir));;
let logdir_mingw32 = logdir ^ "_mingw32";;
let () = ignore (Sys.command ("rm -rf " ^ logdir_mingw32));;

List.iter (fun x -> ignore (Sys.command (Printf.sprintf "test -d %s || mkdir -p %s" x x))) [logdir(*; logdir_mingw32*)];;

type target = {
  name : string; (* no special chars please *)
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
    (fun m (name, cvs_tag, env, (modules : [`native|`mingw32] -> build_module list)) ->
      [
	{
	  name = name;
	 rel_build_dir = name;
	  cvs_dir = Printf.sprintf "%s/%s/%s" cvs_root m name;
	 cvs_tag = cvs_tag;
	  environment = ("LC_ALL", "C") :: ("PKG_CONFIG_PATH", prefix ^ "/lib/pkgconfig:/usr/local/lib/pkgconfig") :: ("LD_LIBRARY_PATH", prefix ^ "/lib") :: ("ACLOCAL_FLAGS", "-I " ^ prefix ^ "/share/aclocal" ^ (if Sys.file_exists (*is_directory*) "/usr/local/share/aclocal" then " -I /usr/local/share/aclocal" else "") ^ " -I /usr/share/aclocal") :: env `native;
	  modules = modules `native;
	  logbase = Printf.sprintf "%s/%s.log." logdir name;
	};
	(*{
	  name = name;
	 rel_build_dir = name ^ "_mingw32";
	  cvs_dir = Printf.sprintf "%s/%s/%s" cvs_root m name;
	 cvs_tag = cvs_tag;
	  environment = ("PKG_CONFIG_LIBDIR", "/usr/local/i386-mingw32/lib/pkgconfig/" (* To ensure we don't pick up native versions unnecessarily. *)) :: ("PKG_CONFIG_PATH", prefix_mingw32 ^ "/lib/pkgconfig:" ^ mingw32msvc_root ^ "/lib/pkgconfig") :: ("LD_LIBRARY_PATH", prefix_mingw32 ^ "/lib" ^ ":" ^ mingw32msvc_root ^ "/lib") :: ("LDFLAGS", "-no-undefined -L" ^ mingw32msvc_root ^ "/lib") :: ("CPPFLAGS", "-I" ^ mingw32msvc_root ^ "/include") :: ("PATH", mingw32msvc_root ^ "/bin:/usr/local/bin:/usr/bin:/bin:/usr/bin/X11:/usr/games") :: ("ACLOCAL_FLAGS", "-I " ^ prefix_mingw32 ^ "/share/aclocal -I " ^ mingw32msvc_root ^ "/share/aclocal " (*-I /usr/local/share/aclocal*) ^ " -I /usr/share/aclocal") :: env `mingw32;
	  modules = modules `mingw32;
	  logbase = Printf.sprintf "%s/%s.log." logdir_mingw32 name;
	}*)
      ]
    )
  in
  let beta m x = gamma ("forge/" ^ m) x in
(*  let sdl_image_LIBS =
    "" (*"-I/win/autobuild/mingw32msvc/target//include/SDL -Dmain=SDL_main -L/win/autobuild/mingw32msvc/target//lib -lmingw32 -lSDLmain -lSDL -mwindows -luser32 -lgdi32 -lwinmm -lSDL_image -lpng -lz"*)
  in*)
(*  gamma "." (target "minimal") @
  gamma "." (target "minimal_empty") @
  gamma "." (target "minimal_warning") @
  gamma "." (target "minimal_error") @*)
    gamma "git" (target "libwfut") @
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
(*	  target "libwfut";*)
	  target (* ~modules:(autotools ~pre:[Link (cvs_root ^ "/forge/protocols/", "../../../../")] ~post:[Unlink "../../../../protocols"]) *) ~env:(fun _ -> ["PYTHONPATH", cvs_root ^ "/forge/libs/Atlas-Python/"]) "Atlas-C++";
(*	  target ~env:(fun _ -> ["PYTHONPATH", cvs_root ^ "/forge/libs/Atlas-Python/"]) ~cvs_tag:(Some "atlas-cpp-0_4_transitional") "Atlas-C++_0_5";*)
	  target "wfmath";
	  target "mercator";
	  target "sage";
(*	  target ~env:(function `mingw32 -> ["LIBS", sdl_image_LIBS] |
`native -> []) "edifice";*)
	  target "eris";
(*	  target ~cvs_tag:(Some "eris_1_2_transitional") "eris_1_2";*)
(*	  target ~modules:(autotools ~configure:(fun x -> match x with `native -> [] | `mingw32 -> ["--without-x"])) "wftk";*)
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
	  target ~modules:(autotools (*~patch:["gcc-3.3.diff"]*)) "cyphesis-C++";
	  target ~modules:(autotools (*~patch:["venus-gcc-3.3.diff"]*)) "venus";
	];

	 "clients",
	 [
(*	  target "feast";*)
(*          target (*~modules:(autotools ~configure:(fun x -> match x with `native -> [] | `mingw32 -> ["--disable-binreloc"]))*) "sear";*)
	  target (*~env:(function `mingw32 -> ["LIBS", sdl_image_LIBS] | `native -> [])*) "equator";
	  target (*~modules:(autotools (*~patch:["apogee-gcc-3.3.diff"]*)) ~env:(function `mingw32 -> ["LIBS", sdl_image_LIBS] | `native -> [])*) "apogee";
(*	  target "uclient";*)
	  target "silence";
	  target "process";
	];
         
       ]
        )
     )
  )
    @ gamma "git" (target "sear")
    @ gamma "git" (target "ember")
 (* @ gamma "." (target "metaserver")*)
(*  @ gamma "." (target "3Drts") XXX think hard *)
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

let ub = (^) "/usr/bin/";;

let no_cvs = true || (Array.length Sys.argv > 1 && Sys.argv.(1) = "--no-cvs") ;;
    
(* CVS update *)
if not no_cvs then
    List.iter
      (fun t ->
	try begin
	  Sys.chdir t.cvs_dir;
	  let cmd = Shell.cmd ~assignments:[Shell.stderr >& Shell.stdout] (ub "cvs") (["-z6"; "update"; "-dP"; "-A"; "-C"] @ (match t.cvs_tag with None -> [] | Some tag -> ["-r"; tag])) in
	  try
	    Shell.call ~stdout:(to_file (t.logbase ^ "cvs")) [cmd]
	  with
            Shell.Subprocess_error res ->
              let meta = open_out (t.logbase ^ "cvs.meta") in
              output_string meta "result:";
              List.iter (dump_evals meta) res;
              close_out meta;
	end with
	  ex ->
	    prerr_endline ("Unable to cvs update " ^ t.cvs_dir ^ " due to unexpected event:\n" ^ (Printexc.to_string ex));
      ) to_build
      ;;

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
       | Cvs_diff -> 
           if no_cvs || cvs_tag <> None then raise (Shell.Subprocess_error ["", Unix.WEXITED 0]);
	   c (ub "cvs") ["-z6"; "diff"; "-u"; "-D"; "24 hours ago" (* XXX check previous diff and use that time *)]
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
          | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _), (Cvs_diff | Make ("check" | "install") | Link _ | Unlink _) -> a
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
            (* generate logs *)
            Sys.chdir real_root;
            ignore (Sys.command "./trurl_render_log ../public_html/");
	  end with
	    ex -> 
	      prerr_endline ("Unable to perform build step due to unexpected event:\n" ^ (Printexc.to_string ex));
	  end;
          Printf.eprintf "debug: module %s end (%s)\n" (string_of_mod m) t.name; flush stderr;
        end;
      ) t.modules;
    Printf.eprintf "debug: build end %s\n" t.name; flush stderr;
     end
  ) to_build
      
(* ./da_log_processor public_html 10 > public_html/index.html.new && mv public_html/index.html.new public_html/index.html *)
