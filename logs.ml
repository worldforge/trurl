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
open Util;;
open ExtLib;;

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


let handlers : (string, cb:Parsers.callbacks -> string -> (result * (int * string))) Hashtbl.t = Hashtbl.create 16;;

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
end;;

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

let pattern_info pattern =
(match pattern with Some (n_tried, rex) -> Printf.sprintf "<!-- %i: %s -->" n_tried rex | None -> "<!-- fallback -->")
;;

let generate_logfile ~tm_module ~t ~target_file file_name : logfile =
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
	match Hashtbl.find_option handlers t with
	    Some h -> begin
      let timing_data = ref None in
      let cb = {
	Parsers.time = (fun data -> timing_data := Some data)
      } in
      let processed_lines =
	List.map
	  (fun (cnt(*, escaped_line*), line) ->
	     let (res, rex) =
	       try let (res, rex) = h ~cb line in res, Some rex with
		   Not_found -> (Unknown, None)
	     in
            (cnt, line, res, rex)
	  ) input_lines
      in

     let o = open_out target_file in
       RenderCommon.html_head ~file:target_file ~title:t o;
       Printf.fprintf o "<div class=\"other\">";
     let in_idx s =
       match s with
           Error | Warning | Dependency_error -> true
         | Information  -> false
         | Unknown -> false (* XXX might need to special case this for hall of confusion *)
      in
     let idx =
        List.filter (fun (_, _, s, _) ->
          in_idx s
                    ) processed_lines
     in
       begin
         Printf.fprintf o "<a href=\"../module_%s.html\">%s</a><br/>" tm_module tm_module;
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
	List.iter (fun (c, l, s, _) ->
          Printf.fprintf o
            "<tt>%03i:&nbsp;</tt><a name=\"index%03i\"><a href=\"#%03i\"><font class=\"%s\">%s</font></a></a><br/>\n"
            c c c (safe_string_of_result s)
            l
                  ) idx;
      end;
      output_string o "<h1>Log</h1>\n";
       Printf.fprintf o "<a href=\"%s\">Raw log (%s).</a><br/>\n" (*source_file*) ("../" ^ source_file) source_file;
      List.iter (fun (c, l, s, pattern) ->
        if in_idx s then
          Printf.fprintf o
            "<tt>%03i:&nbsp;</tt><a name=\"%03i\"><a href=\"#index%03i\"><font class=\"%s\">%s</font></a></a><br/>%s\n"
            c c c (safe_string_of_result s)
            l (pattern_info pattern)
        else
          Printf.fprintf o
            "<tt>%03i:&nbsp;</tt><a name=\"%03i\"><font class=\"%s\">%s</font></a><br/>%s\n"
            c c (safe_string_of_result s)
            l (pattern_info pattern)
                )
        processed_lines;
      Printf.fprintf o "</div>";
      RenderCommon.html_foot ~valid:false o;
      close_out o;

      let x =
        let lst =
          List.map
            (fun (_, _, s, _) ->
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
	       let how, num =
		 try
		   let res = (Pcre.exec ~rex:(Pcre.regexp "^result: (EXIT|SIGNAL|STOP),([0-9]+)$") line) in
		     let how = String.lowercase (Pcre.get_substring res 1) in
		     let num = int_of_string (Pcre.get_substring res 2) in
		       how, num
		 with Not_found ->
		   let res = (Pcre.exec ~rex:(Pcre.regexp "^exit: (\\d+)$") line) in
		     let how = "EXIT" in
		     let num = int_of_string (Pcre.get_substring res 1) in
		       how, num		   
	       in
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
          f_filename = Filename.basename target_file(*, !timing_data*);
	  f_sections = ((*FIXME*) List.map (fun ((_, _, result, _) as line) -> { s_result = result; s_lines = [line]; }) processed_lines); }
	    end 
	  | None ->
	error_endline ("unknown filetype: " ^ t ^ " (" ^ file_name ^ ")");
	{ f_step = t;
          f_result = Unknown;
          f_filename = source_file(*, None*);
          f_sections = ((*FIXME*) List.map (fun ((_, _, result, _) as line) -> { s_result = result; s_lines = [line]; }) (List.map (fun (lineno, (*escaped_*)line(*, _*)) -> (lineno, (*escaped_*)line, Unknown, None)) input_lines)); }
  )
;;

Sys.command "test -d marshal || mkdir marshal";;
Sys.command ("test -d \"" ^ target_dir ^ "/generated\" || mkdir \"" ^ target_dir ^ "/generated\"");;

let process_logfile ~tm_module file_name t =
  let target_file = (Str.global_replace (Str.regexp "/") "_" (file_name) ^ ".html") in
  let inter_file = "marshal/" ^ target_file ^ ".marshal" in
  let target_file = Filename.concat "generated" target_file in
  let target_file = (Filename.concat target_dir target_file) in
  let do_gen () =
    let res = trace ~skip:true ("generate_logfile." ^ tm_module ^ "." ^ t) (generate_logfile ~tm_module ~t ~target_file) file_name in
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

let rsort lst = List.rev (List.sort ~cmp:compare lst);;
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

let parse_log ~tm_module filename =
  let res = (Pcre.exec ~rex:(Pcre.regexp "\\.log\\.([^.]+)$") filename) in
  let step = (Pcre.get_substring res 1) in
    process_logfile ~tm_module filename step
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

let global_ignore_platforms = ["debian-unstable";];;
let global_skip_hosts = ["firefly-virtualbox-ubuntu-lts"];;

let snapshot_merge_raw ({ ts_modules = ts_modules; ts_snapshot = ts_snapshot; } as snapshot : tr_snapshot) (module_name, revision, platform, build, host, checkpoint, logs) : tr_snapshot =
  if (match checkpoint with Some (Some _, Some _) -> false | _ -> true) then (* Skip incomplete builds *)
    snapshot
  else
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
	    ({ tm_snapshot = ts_snapshot; tm_module = module_name; tm_revision = revision; tm_result = Unknown (* FIXME *); tm_platforms = []; tm_building = []; tm_scheduled = []; }, [])
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
	    ({ tp_snapshot = ts_snapshot; tp_module = module_name; tp_platform = platform; tp_result = Unknown; tp_builds = []; }, [])
    in
      find_platform current_module.tm_platforms
  in
  let new_build =
    let (start, time) =
      match checkpoint with
	  Some (Some s, None) ->
	    (s, 0.0)
	| Some (Some s, Some e) ->
	    let (s', _), (e', _) = Unix.mktime s, Unix.mktime e in
	      (s, e' -. s')
	| Some (None, Some _) (* undefined *)
	| Some (None, None)
	| None -> Unix.gmtime 0.0, 0.0
    in
      { tb_build = build; tb_host = host; tb_start = start; tb_time = time; tb_result = fold_result' (fun { f_result = f_result; } -> f_result) logs; tb_logs = logs; }
  in
  let current_platform = { current_platform with tp_builds = List.rev (List.sort ~cmp:(fun a b -> compare a.tb_build b.tb_build) (new_build :: current_platform.tp_builds)); } in
  let current_platform = { current_platform with tp_result = if List.mem current_platform.tp_platform global_ignore_platforms then Unknown else (List.hd current_platform.tp_builds).tb_result (*fold_result' (fun { tb_result = tb_result; } -> tb_result) current_platform.tp_builds*); } in (* FIXME, this should actually be the fold of the tips of the varying hosts, or possibly just the latest build *)
  let current_module = { current_module with tm_platforms = (List.sort ~cmp:(fun a b -> compare a.tp_platform b.tp_platform) (current_platform :: other_platforms)); } in
  let current_module = { current_module with tm_result = fold_result' (fun { tp_result = tp_result; } -> tp_result) current_module.tm_platforms; } in
  let snapshot = { snapshot with ts_modules = current_module :: other_modules; } in
  let snapshot = { snapshot with ts_result = fold_result' (fun { tm_result = tm_result; } -> tm_result) snapshot.ts_modules; } in
    snapshot    
;;

(*
let global_platforms =
  let platforms = Hashtbl.create 32 in
    List.iter
      (fun (k, v) ->
	 Hashtbl.add platforms k v
      ) [];
    platforms
;;
let get_platform_by_host host =
  try
    Hashtbl.find global_platforms host
  with Not_found -> "unknown"
;;
*)

let acc_modules checkpoints_file dir (snapshot : tr_snapshot) revisions platform host build : tr_snapshot =
  let checkpoints =
    let hash = Hashtbl.create 32 in
      begin match checkpoints_file with
	  None -> ()
	| Some file ->
	    let lines = let ch = open_in file in let lst = Std.input_list ch in close_in ch; lst in
	      List.iter
		(fun line ->
		   try
		     let res = (Pcre.exec ~rex:(Pcre.regexp "^([^ ]+) ([^ ]+) ([^ ]+)$") line) in
		     let time = (Pcre.get_substring res 1) in
		     let time' = iso8601_to_tm_utc time in
		     let edge = (Pcre.get_substring res 2) in
		     let module_ = (Pcre.get_substring res 3) in
		     let (s, e) = Hashtbl.find_default hash module_ (None, None) in
		       match edge with
			   "begin" ->
			     Hashtbl.replace hash module_ (Some time', e)
			 | "end" ->
			     Hashtbl.replace hash module_ (s, Some time')
			 | _ -> ()
		   with Not_found -> ()
		) lines
      end;
      hash
  in
  let logs = rlist_dir dir in
  let modules = Hashtbl.create 32 in
    List.iter
      (fun log ->
	   let res = try Some (Pcre.exec ~rex:(Pcre.regexp "^(.+)\\.log\\.[^.]+$") log) with Not_found -> None in
	     match res with
		 None -> ()
	       | Some res ->
		   let module_ = (Pcre.get_substring res 1) in
		     hash_add_to_list modules module_ (parse_log ~tm_module:module_ (dir ^ "/" ^ log));
      ) logs;
    Hashtbl.fold
      (fun k v snapshot ->
	 snapshot_merge_raw snapshot (k, Hashtbl.find revisions k, platform, build, host, Hashtbl.find_option checkpoints k, v)
      ) modules snapshot
;;

let load_platform base =
  let platform =
    let lines =
      try
	let ch = open_in (base ^ "/platform") in
	let lst = Std.input_list ch in
	  close_in ch;
	  lst
      with Sys_error _ -> []
    in
      (* FIXME, list.find matching *)
    let find_group_in_list rex group lst =
      Pcre.get_substring
	(Pcre.exec
	   ~rex
	   (List.find
	      (fun line ->
		 try
		   ignore (Pcre.exec ~rex line);
		   true
		 with
		     Not_found ->
		       false
	      ) lst))
	group
    in
      try
	let distributor = find_group_in_list (Pcre.regexp "^Distributor ID:\\s+(.+)$") 1 lines in
	let release = find_group_in_list (Pcre.regexp "^Release:\\s+(.+)$") 1 lines in
	  (String.lowercase distributor) ^ "-" ^ (String.lowercase release)
      with Not_found ->
	"unknown" (*get_platform_by_host host (* FIXME, this is quite a hack *)*)
  in
    platform
;;

let acc_builds dir snapshot revisions host : tr_snapshot =
  let builds = rsort (list_dir dir) in
  let builds =
    List.fold_left
      (fun (n, snapshot) build ->
	 let base = (dir ^ "/" ^ build) in
	 let modules_dir =
	   if Sys.file_exists (base ^ "/modules") then
	     (base ^ "/modules")
	   else base
	 in
	 let checkpoints =
	   if Sys.file_exists (base ^ "/checkpoints") then
	     Some (base ^ "/checkpoints")
	   else None
	 in
	 let platform = load_platform base in
	   if n > 3 then (* FIXME, use lazy and possibly build list info and only reparse those that are missing *)
	     (n, snapshot)
	   else
	     (n + 1, trace ~skip:true ("parse.build." ^ build) (acc_modules checkpoints modules_dir snapshot revisions platform host) (int_of_string build))
      ) (0, snapshot) builds
  in snd builds
;;

let acc_hosts dir snapshot revisions : tr_snapshot =
  let hosts = rsort (list_dir dir) in
    List.fold_left
      (fun snapshot host ->
	 if List.mem host global_skip_hosts then
	   snapshot
	 else
	   trace ~skip:true ("parse.host." ^ host) (acc_builds (dir ^ "/" ^ host ^ "/builds") snapshot revisions) host
      ) snapshot hosts
;;

let acc_snapshots dir : tr_snapshot list =
  let acc =
    let n_acc_snapshots = ref 0 in
    let years = rsort (list_dir dir) in
      List.map
	(fun year ->
	   if !n_acc_snapshots >= snapshot_hard_limit (*FIXME, instead use lazy *) then
	     []
	   else
	     let months = rsort (list_dir (dir ^ "/" ^ year)) in
	       List.map
		 (fun month ->
		    if !n_acc_snapshots >= snapshot_hard_limit (*FIXME, instead use lazy *) then
		      []
		    else
		      let days = rlist_dir (dir ^ "/" ^ year ^ "/" ^ month) in
			List.map
			  (fun day ->
			     if !n_acc_snapshots >= snapshot_hard_limit (*FIXME, instead use lazy *) then
			       []
			     else
			       let snaps = rlist_dir (dir ^ "/" ^ year ^ "/" ^ month ^ "/" ^ day) in
				 List.map
				   (fun snap ->
				      let snapshot = (int_of_string year, int_of_string month, int_of_string day, int_of_string snap) in
				      let snapshot_dir = dir ^ "/" ^ year ^ "/" ^ month ^ "/" ^ day ^ "/" ^ snap in
				      let revisions_file = (snapshot_dir ^ "/revisions") in
				      let hosts_dir = (snapshot_dir ^ "/" ^ "hosts") in
					if !n_acc_snapshots >= snapshot_hard_limit || not (Sys.file_exists revisions_file) || not (Sys.file_exists hosts_dir) then
					  []
					else
					  ( incr n_acc_snapshots;
					    let revisions = parse_revisions revisions_file in
					      [trace ("parse.snapshot." ^ (snapshot_safe_string snapshot)) ((*FIXME*)acc_hosts hosts_dir { ts_snapshot = snapshot; ts_result = Unknown; ts_modules = []; }) revisions] )
				   ) (snaps)
			  ) (days)
		 ) (months)
	) (years)
  in
    (List.concat (List.concat (List.concat (List.concat acc))))
;;

let acc_logdata' () : tr_snapshot list =
  trace "parse.snapshot" acc_snapshots "logs"
;;

let merge_building_with_snapshot snapshots snapshot_id m building =
  let rec merge acc lst =
    match lst with
	[] -> List.rev acc
      | ({ ts_snapshot = ts_snapshot; ts_modules = ts_modules; } as hd) :: tl ->
	  if ts_snapshot = snapshot_id then
	    let rec merge' acc lst =
	      match lst with
		  [] -> List.rev acc
		| ({ tm_module = tm_module; tm_building = tm_building; } as hd) :: tl ->
		    if tm_module = m then
		      merge' ({ hd with tm_building = (building :: tm_building) } :: acc) tl
		    else merge' (hd :: acc) tl
	    in
	      merge ({ hd with ts_modules = merge' [] ts_modules } :: acc) tl
	  else merge (hd :: acc) tl
  in merge [] snapshots
;;

let live_is_building_rex = (Pcre.regexp "^\\d+-\\d+-\\d+T\\d+:\\d+:\\d+(\\+00:00|Z) (begin|end) ([^ ]+)");;
let add_live_data (snapshots : tr_snapshot list) =
  let root, snapshot_list =
    List.fold_left
      (fun (acc, lst) () ->
	 let entry = (List.hd (rsort (list_dir acc))) in
	   (acc ^ "/" ^ entry, entry :: lst)
      ) ("/home/trurl/work/logs", []) [(); (); (); ()] (* year, month, day, snap *)
  in (* FIXME look back three steps *)
    (fun root snapshot_list ->
  let snapshot_id =
    match snapshot_list with
	[snap; day; month; year] ->
	  (int_of_string year, int_of_string month, int_of_string day, int_of_string snap)
      | _ ->
	  (0, 0, 0, 0) (* This cannot happen[tm]. *)
  in
  let root_hosts = root ^ "/hosts" in
    
  let hosts = try Some (list_dir root_hosts) with Unix.Unix_error _ -> None in
    match hosts with
	Some hosts ->
      let snapshots =
	List.fold_left
	  (fun snapshots host ->
	     let root_builds = root_hosts ^ "/" ^ host ^ "/builds" in
	       if (Sys.file_exists root_builds) then
		 let build = (List.hd (rsort (list_dir root_builds))) in
		 let root_build = root_builds ^ "/" ^ build in
		 let checkpoints_path = root_build ^ "/checkpoints" in
		   if (Sys.file_exists checkpoints_path) then
		     let lines =
		       let ch = open_in checkpoints_path in
		       let lst = Std.input_list ch in
			 close_in ch;
			 lst
		     in
		       (*
			 ^date begin module$ -> building
			 ^date end module$ -> finished
			 remainder -> scheduled
		       *)
		     let last = List.last lines in
		     let module_ =
		       try
			 let res = Pcre.exec ~rex:live_is_building_rex last in
			   Some (Pcre.get_substring res 3)
		       with Not_found -> None
		     in
		       match module_ with
			   None -> snapshots
			 | Some m ->
			     let platform = load_platform root_build in
			       merge_building_with_snapshot snapshots snapshot_id m { tmb_platform = platform; tmb_host = host; }
		   else snapshots
	       else snapshots
	  ) snapshots hosts;
      in
	(snapshots)
      | None ->
	  (snapshots)
    ) root snapshot_list
;;

let merge_tip (tip : tr_module list) (snapshot : tr_snapshot) =
  List.fold_left
    (fun tip_modules snap_module ->
	   let rec merge acc lst =
	     match lst with
		 [] ->
		   ( snap_module :: tip_modules )
	       | hd :: tl ->
		   if hd.tm_module = snap_module.tm_module then
		     let tm_platforms =
		       let tip_platforms, snap_platforms =
			 hd.tm_platforms, snap_module.tm_platforms
		       in
			 tip_platforms @
			   (List.filter
			      (fun s_p ->
				 not (List.exists (fun t_p -> t_p.tp_platform = s_p.tp_platform) tip_platforms)
			      ) snap_platforms)
		     in
		       { hd with
			   tm_platforms = tm_platforms;
			   tm_result = AlertSort.fold_result_platforms tm_platforms;
		       } :: tl @ acc
		   else
		     merge (hd :: acc) tl
	   in merge [] tip_modules
    ) tip snapshot.ts_modules
;;

let calc_tip snapshots =
  List.fold_left
    merge_tip
    [] snapshots
;;

let main () =

  let snapshots =
    trace "parse" (fun () ->
		     let snapshots = acc_logdata' () in
		     let snapshots =
		       try
			 trace "parse.live" add_live_data snapshots
		       with _ -> (* FIXME, print exception *)
			 prerr_endline "Error adding live data.";
			 snapshots
		     in
		       snapshots) ()
  in

  let logs, module_hash =
    trace "calc" (fun () ->
		    let logs =
		      { tl_snapshots = snapshots;
			tl_tip = trace "calc.tip" calc_tip snapshots; }
		    in
		    let module_hash = trace "calc.module_hash" RenderCommon.calc_module_hash (logs.tl_snapshots) in
		      logs, module_hash) ()
  in
    (* FIXME Calc scheduled module/platform/host combinationa *)
    (* Sort: platforms, snapshots, modules, ... FIXME *)
    (* FIXME Generate tip, hash and array *)

    trace "render" (fun () ->
		      trace "render.project_pages" Render.project_pages logs;
		      
		      trace "render.snapshots" Render.snapshots logs;
		      
		      let current_modules = logs.tl_tip in
			trace "render.frontpage" (Render.frontpage logs module_hash) current_modules;
			trace "render.snapshot_module_platform" (List.iter RenderCommon.snapshot_module_platform_href_do_render) (!RenderCommon.snapshot_module_platform_to_render);
			
			let old_results = Results.load () in
			let new_results, changeset =
			  Results.merge old_results current_modules (*logs*)
			in
			  Results.save new_results;
			  Results.hooks changeset current_modules (*logs*);
		   ) ()
;;

trace "main" main ()
