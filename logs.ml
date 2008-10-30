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

(* /usr/share/pixmaps/gnome-pkgview <- useful logos (most likely fold all working into the linux/unix flag) *)

open Types;;
open Config;;

let find_special (logs : logfile list) =
  let rec aux lst =
    match lst with
        [] -> None
      | { f_special = hd } :: tl ->
          match hd with
              None -> aux tl
            | Some _ -> hd
  in aux logs
;;

let find_timing logs =
  match find_special logs with
      None -> None
    | Some Timing x -> Some x
;;

let fold_result l = (List.fold_left AlertSort.highest_alert Unknown) l;;
let fold_result' f l = fold_result (List.map f l);;

let list_find_map (f : 'a -> 'b option) (lst : 'a list) : 'b =
  match
    List.find
      (fun x -> (f x) <> None)
      lst
  with
    None -> assert false
  | Some x -> x

let rules_filename = "trurl_log_rules.xml"
let stat_self = Unix.stat Sys.argv.(0)
let stat_rules = Unix.stat rules_filename

let partial =
  try
    Sys.argv.(2) = "-partial" || Sys.argv.(3) = "-partial"
  with
    Invalid_argument "Array.get"
  | Invalid_argument "index out of bounds" -> false
;;

let iter_pair f lst =
  List.iter
    (fun (x,y) -> f x y)
    lst
;;

let build_module_page () = ()
let build_module_date_page () = ()

let logfile_missing = Unknown, "no log";;

let fn_cat lst = List.fold_left Filename.concat "" lst;;


let handlers : (string, cb:Parsers.callbacks -> string -> result) Hashtbl.t = Hashtbl.create 16;;

exception PCDATA;;
begin try
  let rule_hash = Hashtbl.create 16 in
  let get_element node = match node with Xml.Element x -> x | Xml.PCData _ -> raise PCDATA in
  begin
    let rules = Xml.parse_file rules_filename in
    let tag_name, attributes, children = get_element rules in
    List.iter
      (fun logtype ->
	let tag_name, attributes, children = get_element logtype in
	let extension = snd (List.find (fun (name, value) -> name = "extension") attributes) in
	(Hashtbl.add handlers)
	  (extension)
	  begin
	    let prepend =
	      if (List.exists (fun (name, value) -> name = "prepend_rules") attributes) then begin
		let reference = (snd (List.find (fun (name, value) -> name = "prepend_rules") attributes)) in
		Hashtbl.find rule_hash reference
	      end else []
	    in
	    let rule_list = 
	      List.flatten
		(List.map
		  (fun rule ->
		    let tag_name, attributes, children = get_element rule in
		    let expanded_rules = begin
		      try
			[snd (List.find (fun (name, value) -> name = "regexp") attributes)]
		      with Not_found ->
			(* Use PCDATA *)
			match children with (* TODO, not as regexp *)
			  [Xml.PCData text] ->
			    let fragments = ExtString.String.nsplit text "\n" in
			    (* ignore pure whitespace *)
			    let fragments = List.filter
			      (fun frag ->
				try
				  ignore (Pcre.exec ~rex:(Pcre.regexp "^\\s*$") frag);
				  debug_endline ("Ignoring fragment '" ^ frag ^ "'.");
				  false
				with Not_found ->
				  true
			      ) fragments
			    in
			    List.map (fun frag -> "^" ^ (Pcre.quote frag) ^ "$") fragments
			| [] ->
			    error_endline ("Neither regular expression nor PCDATA."); []
			| _ ->
			    error_endline ("Non-PCDATA nodes inside <rule>."); []
		    end in
		    let result =
		      match snd (List.find (fun (name, value) -> name = "result") attributes) with
			(*Perfect - not valid, only derived *)
		      
		      "Error" -> Error
		    | "Dependency_error" -> Dependency_error
		    | "Warning" -> Warning
		    | "Information" -> Information
		    | "Unknown" -> error_endline ("Unknown is not a valid result (folded into Unknown)"); Unknown
		    | result -> error_endline ("Unknown result: " ^ result ^ " (folded into Unknown)"); Unknown
		    in
		    List.map (fun rule -> rule, result) expanded_rules
		  ) children)
	    in
	    let rule_list = prepend @ rule_list in
	    Hashtbl.add rule_hash extension rule_list;
	    (
	      Parsers.__parse begin
		rule_list
	      end
	    )
	  end
      ) children
  end
with
  PCDATA ->
    error_endline ("PCDATA among rules!")
end

let type_names = ["timing", "time";"cvs", "cvs update"; "diff", "diff -u"; "patch", "patch"; "autogen_sh", "./autogen.sh"; "configure", "./configure"; "make_all", "make all"; "make_install", "make install"; "make_check", "make check"]
let type_name_hash = Hashtbl.create 16;;
iter_pair (Hashtbl.add type_name_hash) type_names
let get_type_name x =
  try
    Hashtbl.find type_name_hash x
  with
    Not_found -> x
let type_order = List.map fst type_names

let log_regexp = Str.regexp "^\\(.*\\)\\.log\\.\\([^.~]+\\)$";;
let match_log s f =
  if Str.string_match log_regexp s 0 then
    let dir = Str.matched_group 1 s in
    let data = Str.matched_group 2 s in
    f dir data
;;
    
type line_count = {
    error : int;
    warning : int;
    dependency_error : int;
    good : int;
    don't_care : int;
    not_our_fault : int;
    induced_warning : int;
    add : int;
    delete : int;
    time_begin : int;
    time_end : int;
    unmatched : int;
  }

let specialized_information t input_lines =
  match t with
      "timing" ->
        if List.length input_lines > 1 then
          ( let imperative_timing, timing = ref None, ref (Unix.gmtime 0.0, 0.0) in
              List.iter
                (fun (_, i) ->
                   try
                   Scanf.sscanf i "BUILD %s %s %i-%i-%i %i:%i:%i"
                     (fun state target year mon mday hour min sec ->
	                let (t, t') = Unix.mktime {
	                  Unix.tm_sec = sec;
   	                  Unix.tm_min = min;
   	                  Unix.tm_hour = hour;
   	                  Unix.tm_mday = mday;
   	                  Unix.tm_mon = mon - 1;
   	                  Unix.tm_year = year - 1900;
	                  (* ignored *)
   	                  Unix.tm_wday = 0;
   	                  Unix.tm_yday = 0;
   	                  Unix.tm_isdst = false;
	                } in
	                  match state with
	                      "BEGIN" ->
				timing := (t', 0.0);
	                        imperative_timing := Some t
	                    | "END" ->
	                        (match !imperative_timing with
	                             Some ot ->
		                       let tdiff = t -. ot in
		                         timing := (fst (!timing), tdiff);
		                         imperative_timing := None
	                           | None -> error_endline "Timing.END but no previous time")
	                    | _ ->
                                ()
                     )
                   with _ -> error_endline "Failed scanning Timing"
                ) input_lines;
          Some (Timing (!timing)) (* FIXME *) )
        else None
    | _ -> None
;;

let generate_logfile ~t ~target_file file_name : logfile =
  (
    let source_file = file_name in
    let input_lines =
      let i = (open_in source_file) in
      begin (* process lines *)
        let pl = ref [] in
        let cnt = ref 0 in
        try
          while true do
            incr cnt;
 	    let line =
              input_line i
            in
            pl := (!cnt(*, (escape_html line)*), line(*,
            try h ~cb line with
              Not_found -> Unmatched
*)) :: !pl;
          done;
          failwith "this can't happend";
        with End_of_file ->
	  close_in i;
          List.rev !pl
      end;
    in
      try
      let timing_data = ref None in
      let cb = {
	Parsers.time = (fun data -> timing_data := Some data)
      } in
      let h = Hashtbl.find handlers t in
      let processed_lines =
	List.map
	  (fun (cnt(*, escaped_line*), line) ->
            (cnt, (*escaped_*)line,
	    try h ~cb line with
              Not_found -> Unknown)
	  ) input_lines
      in

     let o = open_out target_file in
       RenderCommon.html_head ~ch:o ~file:target_file ~title:t;
       Printf.fprintf o "<div class=\"other\">";
     let in_idx s =
       match s with
           Error | Warning | Dependency_error -> true
         | Information  -> false
         | Unknown -> false (* XXX might need to special case this for hall of confusion *)
      in
     let idx =
        List.filter (fun (_, _, s) ->
          in_idx s
                    ) processed_lines
     in
       begin
         Printf.fprintf o "Steps: ";
         List.iter
           (fun step ->
              let target2 =
                Pcre.replace
                  ~rex:(Pcre.regexp "\\.log\\.[-a-z_]+\\.html$")
                  ~templ:".log."
                  target_file
              in
              let target2 = target2 ^ step ^ ".html" in
                if target2 = target_file then
                  Printf.fprintf o " <b>%s</b>" step
                else
                  let target2 =
                    Pcre.replace
                      ~rex:(Pcre.regexp "^\\.\\./public_html/")
                      ~templ:"../"
                      target2
                  in
                    Printf.fprintf o " <a href=\"%s\">%s</a>" target2 step
           ) ["timing"; "autogen_sh"; "configure"; "make_all"; "make_check"; "make_install"];
         Printf.fprintf o "<br />";
       end;
     begin
        try
          let m = open_in (file_name ^ ".meta") in
          output_string o "<tt>META:<br />\n";
          Pcre.foreach_line ~ic:m
            (fun line ->
               (if line = "result: EXIT,0" then
                  output_string o "<font class=\"information\">"
                else
                  output_string o "<font class=\"error\">");
              output_string o line;
              output_string o "</font><br />";
            );
          output_string o "</tt>\n";
          close_in m;
        with
          Sys_error _ -> ()
      end;
     if List.length idx > 0 then begin
	output_string o "<h1>Index</h1>\n";
	List.iter (fun (c, l, s) ->
          Printf.fprintf o
            "<tt>%03i:&nbsp;</tt><a name=\"index%03i\"><a href=\"#%03i\"><font class=\"%s\">%s</font></a></a><br>"
            c c c (safe_string_of_result s)
            l
                  ) idx;
      end;
      output_string o "<h1>Log</h1>";
       Printf.fprintf o "<a href=\"%s\">Raw log (%s).</a><br>\n" (*source_file*) ("../" ^ source_file) source_file;
      List.iter (fun (c, l, s) ->
        if in_idx s then
          Printf.fprintf o
            "<tt>%03i:&nbsp;</tt><a name=\"%03i\"><a href=\"#index%03i\"><font class=\"%s\">%s</font></a></a><br>"
            c c c (safe_string_of_result s)
            l
        else
          Printf.fprintf o
            "<tt>%03i:&nbsp;</tt><a name=\"%03i\"><font class=\"%s\">%s</font></a><br>"
            c c (safe_string_of_result s)
            l
                )
        processed_lines;
      Printf.fprintf o "</div>";
      RenderCommon.html_foot ~valid:false o;
      close_out o;

      let x =
        let lst =
          List.map
            (fun (_, _, s) ->
	      s
            ) processed_lines
        in
	List.fold_left AlertSort.highest_alert Unknown lst in
      let x =
        try
          let m = open_in (file_name ^ ".meta") in
          let rval = ref Unknown in
          Pcre.foreach_line ~ic:m
            (fun line ->
              let res = (Pcre.exec ~rex:(Pcre.regexp "result: (EXIT|SIGNAL|STOP),([0-9]+)") line) in
              let how = (Pcre.get_substring res 1) in
              let num = int_of_string (Pcre.get_substring res 2) in
              rval :=
                match t, how, num with
                  _, "EXIT", 0
                | "diff", "EXIT", 1 -> !rval
                | "cvs", "EXIT", 1 -> AlertSort.highest_alert !rval Warning
                | "patch", "EXIT", 1 -> AlertSort.highest_alert !rval Warning
                | "autogen_sh", "EXIT", 1
                | "configure", "EXIT", 1
                | "autogen_sh", "EXIT", 127 -> AlertSort.highest_alert !rval Dependency_error
                | _, _, _ -> AlertSort.highest_alert !rval Error
            );
          close_in m;
          AlertSort.highest_alert !rval x
        with
          Sys_error _ -> x
      in
        { f_step = t;
          f_result = x;
          f_special = (specialized_information t input_lines);
          f_filename = Filename.basename target_file(*, !timing_data*);
          f_lines = processed_lines; }
    with
      Not_found ->
	error_endline ("unknown filetype: " ^ t ^ " (" ^ file_name ^ ")");
	{ f_step = t;
          f_result = Unknown;
          f_special = None;
          f_filename = source_file(*, None*);
          f_lines = (List.map (fun (lineno, (*escaped_*)line(*, _*)) -> (lineno, (*escaped_*)line, Unknown)) input_lines); }
  )
;;

Sys.command "test -d marshal || mkdir marshal";;
Sys.command ("test -d \"" ^ target_dir ^ "/generated\" || mkdir \"" ^ target_dir ^ "/generated\"");;

let process_logfile file_name t =
  let target_file = (Str.global_replace (Str.regexp "/") "_" (file_name) ^ ".html") in
  let inter_file = "marshal/" ^ target_file ^ ".marshal" in
  let target_file = Filename.concat "generated" target_file in
  let target_file = (Filename.concat target_dir target_file) in
  let do_gen () =
    let res = generate_logfile ~t ~target_file file_name in
    let oc = (open_out inter_file) in
    Marshal.to_channel oc res [];
    close_out oc;
    res
  in
  try
    let stat_source, stat_target, stat_inter, stat_self, stat_rules =
      Unix.stat file_name, Unix.stat target_file, Unix.stat inter_file, stat_self, stat_rules
    in
    if stat_source.Unix.st_mtime < stat_target.Unix.st_mtime && stat_source.Unix.st_mtime < stat_inter.Unix.st_mtime && stat_self.Unix.st_mtime < stat_inter.Unix.st_mtime && stat_rules.Unix.st_mtime < stat_inter.Unix.st_mtime then begin
      let ch = (open_in inter_file) in
      let res = Marshal.from_channel ch in
      close_in ch;
      res
    end else do_gen ()
  with
    Unix.Unix_error (Unix.ENOENT, _, _) (*as err*) ->
      do_gen ()

let generate_logdir =
  (fun (dir_name, snapshot) ->
(*    let glance = ref [] in*)
    let __fields = ref [] in
    let reg_field f =
      if not (List.mem f !__fields) then
	__fields := f :: !__fields;
      in
(*    let date_buf = Buffer.create 1024 in*)
    let date_dir = Unix.opendir dir_name in
    let files = Hashtbl.create 16 in
    (try
      while true do
	let file_name = Unix.readdir date_dir in
	match_log file_name
          (fun dir data ->
	    reg_field data;
	    if Hashtbl.mem files dir then
	      Hashtbl.replace files dir ((data, file_name) :: (Hashtbl.find files dir))
	    else
	      Hashtbl.add files dir [data, file_name]
	  )
      done
    with
      End_of_file -> ());
    Unix.closedir date_dir;
    let output = ref [] in
    Hashtbl.iter
      (fun dir lst ->
	output := (dir,
	(List.map (fun (t, f) -> process_logfile (Filename.concat dir_name f) t) lst)) :: !output;
      ) files;
    let glance =
      List.map
        (fun (project, data) ->
	   { l_module = project;
	     l_result = (List.fold_left AlertSort.highest_alert Unknown (List.map (fun { f_result = result } -> result) data));
             l_logs = data;
	     l_snapshot = snapshot; }
        ) !output;
      in
    (glance : logs)
  )
let process_logdir = generate_logdir

let acc_logdata () (*: (string option * ((string * result * logs) list) list) list*) =
  let build_logs_name = "logs" in
  let file_acc = Hashtbl.create 16 in

  let rec walkndirs dirname maxdepth n leaf_function =
    let build_logs_dir = Unix.opendir (Filename.concat build_logs_name dirname) in
      (try
       while true do
         let date_name = Unix.readdir build_logs_dir in
           match date_name with
               "." | ".." -> ()
             | entry ->
                 let name = Filename.concat dirname entry in
                   if n >= maxdepth
                   then leaf_function name
                   else walkndirs name maxdepth (n + 1) leaf_function 
       done
     with
         End_of_file -> (Unix.closedir build_logs_dir))
  in
    walkndirs "" 3 0
      (fun name ->
	 let snapshot = Scanf.sscanf name "%i/%i/%i/%i" (fun year month day build -> (year, month, day, build)) in
	 let name = name ^ "/logs" in
	 let build = None in
	   if Hashtbl.mem file_acc build then
	     Hashtbl.replace file_acc build
	       ((name, snapshot) :: Hashtbl.find file_acc build)
	   else
	     Hashtbl.add file_acc build [(name, snapshot)]
      );

  let process_acc file_acc : logs list (*((string * result * logs) list) list*) =
    let file_acc =
      let int = ref (List.rev (List.sort (fun x y -> compare x y) file_acc)) in
      let ext = ref [] in
      for i=1 to generate_n_logs do
	match !int with
	  [] -> ()
	| hd :: tl ->
	    ext := hd :: !ext;
	    int := tl
      done;
      List.rev !ext
    in
      List.map
        (fun (x, snapshot) ->
	   (fun (year, month, day, build) -> Printf.eprintf "(%i, %i, %i, %i)\n" year month day build) snapshot;
           (process_logdir (Filename.concat build_logs_name x, snapshot))
        ) file_acc
  in
  let out_acc = ref [] in
  Hashtbl.iter
    (fun key data ->
      out_acc := (key, (process_acc data)) :: !out_acc
    ) file_acc;
  !out_acc
;;

let rsort lst = List.rev (List.sort compare lst);;
let list_dir dir =
  let lst = ref [] in
  let dir' = Unix.opendir dir in
    (try
       while true do
         let filename = Unix.readdir dir' in
           match filename with
               "." | ".." -> ()
             | entry ->
		 lst := entry :: !lst
       done
     with
         End_of_file -> (Unix.closedir dir'));
    !lst
;;
let rlist_dir dir =
  rsort (list_dir dir)
;;
let hd = List.hd;;

let parse_revisions file =
(*  let channel = open_in file in*)
  let scanbuf = Scanf.Scanning.from_file file in
  let revisions = Hashtbl.create 32 in
  let acc = Hashtbl.create 16 in
    begin try
      while true do
	(try
	   Scanf.bscanf scanbuf "\n" (fun () -> () (*print_endline "new section"*)) ();
	   let f = Hashtbl.find acc in
	   let name = Hashtbl.find acc "name" in
	   let vcs =
	     match Hashtbl.find acc "vcs" with
		 "cvs" ->
		   CVS { vc_repository = f "cvs.repository"; vc_path = f "cvs.path"; vc_branch = f "cvs.branch"; vc_time = Scanf.sscanf (f "cvs.time") "%i-%i-%i %i:%i:%i" (fun year month day hour minute second -> { Unix.tm_year = year - 1900; tm_mon = month - 1; tm_mday = day; tm_hour = hour; tm_min = minute; tm_sec = second; (* following is ignored: *) tm_wday = 0; tm_yday = 0; tm_isdst = false; }); }
	       | "git" ->
		   Git { vg_repository = f "git.repository"; vg_branch = f "git.branch"; vg_commit = f "git.commit"; }
	       | vcs -> failwith ("unknown vcs: " ^ vcs)
	   in
	     Hashtbl.add revisions name vcs;
	     Hashtbl.clear acc;
	 with
	     Scanf.Scan_failure _ ->
	       Scanf.bscanf scanbuf "%[^ ] %[^\n]\n" (fun k v -> (*Printf.printf "k: '%s' v: '%s'\n" k v;*) Hashtbl.add acc k v));
      done
    with End_of_file ->
      if Hashtbl.length acc <> 0 then failwith "unclaimed section FIXME";
    end;
    revisions
(*    close_in channel;*)
    
;;

let parse_log filename =
  let res = (Pcre.exec ~rex:(Pcre.regexp "\\.log\\.([^.]+)$") filename) in
  let step = (Pcre.get_substring res 1) in
    process_logfile filename step
;;

let hash_add_to_list hash key item =
  if not (Hashtbl.mem hash key) then
    Hashtbl.add hash key [item]
  else
    Hashtbl.replace hash key (item :: Hashtbl.find hash key)
;;

let vcs_same_branch vcs vcs' =
  match vcs, vcs' with
      CVS { vc_branch = vc_branch }, CVS { vc_branch = vc_branch' } ->
	vc_branch = vc_branch'
    | Subversion _, Subversion _ -> failwith "FIXME vcs_same_branch subversion"
    | Git { vg_branch = vg_branch }, Git { vg_branch = vg_branch' } ->
	vg_branch = vg_branch'
    | _ -> failwith "vcs_same_branch with different vcs called"
;;

let snapshot_merge_raw ({ ts_modules = ts_modules; } as snapshot : tr_snapshot) (module_name, revision, platform, build, host, logs) : tr_snapshot =
  let current_module, other_modules =
    let rec find_module lst =
      match lst with
	  { tm_module = tm_module; tm_revision = tm_revision; } as hd :: tl ->
	    if (module_name = tm_module) && (vcs_same_branch tm_revision revision) then
	      (hd, tl)
	    else
 	      let (hd', tl') = find_module tl in
		(hd', hd :: tl')
	| [] ->
	    ({ tm_module = module_name; tm_revision = revision; tm_result = Unknown (* FIXME *); tm_platforms = []; }, [])
    in
      find_module ts_modules
  in
  let current_platform, other_platforms =
    let rec find_platform lst =
      match lst with
	  { tp_platform = tp_platform } as hd :: tl ->
	    if (platform = tp_platform) then
	      (hd, tl)
	    else
 	      let (hd', tl') = find_platform tl in
		(hd', hd :: tl')
	| [] ->
	    ({ tp_platform = platform; tp_result = Unknown; tp_builds = []; }, [])
    in
      find_platform current_module.tm_platforms
  in
  let new_build =
    let (start, time) =
      match find_timing logs with
	  None -> Unix.gmtime 0.0, 0.0
	| Some t -> t
    in
    { tb_build = build; tb_host = host; tb_start = start; tb_time = time; tb_result = fold_result' (fun { f_result = f_result; } -> f_result) logs; tb_logs = logs; }
  in
  let current_platform = { current_platform with tp_builds = new_build :: current_platform.tp_builds; } in
  let current_platform = { current_platform with tp_result = fold_result' (fun { tb_result = tb_result; } -> tb_result) current_platform.tp_builds; } in
  let current_module = { current_module with tm_platforms = current_platform :: other_platforms; } in
  let current_module = { current_module with tm_result = fold_result' (fun { tp_result = tp_result; } -> tp_result) current_module.tm_platforms; } in
  let snapshot = { snapshot with ts_modules = current_module :: other_modules; } in
  let snapshot = { snapshot with ts_result = fold_result' (fun { tm_result = tm_result; } -> tm_result) snapshot.ts_modules; } in
    snapshot    
;;

let platforms = Hashtbl.create 32;;
List.iter
  (fun (k, v) ->
     Hashtbl.add platforms k v
  ) ["demitar-cat", "ubuntu-8.04"] (* FIXME, load from file *)
;;

let acc_modules dir (snapshot : tr_snapshot) revisions host build : tr_snapshot =
  let logs = rlist_dir dir in
  let modules = Hashtbl.create 32 in
    List.iter
      (fun log ->
	 try
	   let res = (Pcre.exec ~rex:(Pcre.regexp "^(.+)\\.log\\.[^.]+$") log) in
	   let module_ = (Pcre.get_substring res 1) in
	     hash_add_to_list modules module_ (parse_log (dir ^ "/" ^ log));
	 with Not_found -> ()
      ) logs;
    Hashtbl.fold
      (fun k v snapshot ->
	 snapshot_merge_raw snapshot (k, Hashtbl.find revisions k, Hashtbl.find platforms host, build, host, v)
      ) modules snapshot
;;

let acc_builds dir snapshot revisions host : tr_snapshot =
  let builds = rsort (list_dir dir) in
    List.fold_left
      (fun snapshot build ->
	 acc_modules (dir ^ "/" ^ build) snapshot revisions host (int_of_string build)
      ) snapshot builds
;;

let acc_hosts dir snapshot revisions : tr_snapshot =
  let hosts = rsort (list_dir dir) in
    List.fold_left
      (fun snapshot host ->
	 acc_builds (dir ^ "/" ^ host ^ "/builds") snapshot revisions host
      ) snapshot hosts
;;

let acc_snapshots dir : tr_logs =
  let acc =
  let years = rsort (list_dir dir) in
    List.map
      (fun year ->
       let months = rsort (list_dir (dir ^ "/" ^ year)) in
	 List.map
	   (fun month ->
	    let days = rlist_dir (dir ^ "/" ^ year ^ "/" ^ month) in
	      List.map
		(fun day ->
		 let snaps = rlist_dir (dir ^ "/" ^ year ^ "/" ^ month ^ "/" ^ day) in
		   List.map
		     (fun snap ->
			let snapshot = (int_of_string year, int_of_string month, int_of_string day, int_of_string snap) in
			let snapshot_dir = dir ^ "/" ^ year ^ "/" ^ month ^ "/" ^ day ^ "/" ^ snap in
			let revisions = parse_revisions (snapshot_dir ^ "/" ^ "revisions") in
			  [(*FIXME*)acc_hosts (snapshot_dir ^ "/" ^ "hosts") { ts_snapshot = snapshot; ts_result = Unknown; ts_modules = []; } revisions]
		     ) (snaps)
	      ) (days)
	 ) (months)
    ) (years)
  in
    (List.concat (List.concat (List.concat (List.concat acc))))
;;

let acc_logdata' () : tr_logs =
  acc_snapshots "logs"
;;

let main () =

  let snapshots = acc_logdata' () in

  Render.project_pages snapshots;

  Render.snapshots snapshots;
  Render.frontpage snapshots;

  let old_results = Results.load () in
  let new_results, changeset =
    Results.merge old_results snapshots
  in
    Results.save new_results;
    Results.hooks changeset snapshots;
;;

main ()
