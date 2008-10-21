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

(*type status = Error | Warning | Dependency_error | Good | Don't_care | Not_our_fault | Induced_warning | Add | Delete | Time_begin | Time_end | Unmatched
type result = Failed | Unsatisfied_dependencies | Warned | Known_error | Probably_ok | Perfect | Information | Unknown*)

(*let safe_string_of_status s =
  match s with
    Error -> "error"
  | Warning -> "warning"
  | Dependency_error -> "dependency_error"
  | Good -> "good"
  | Don't_care -> "dont_care"
  | Not_our_fault -> "not_our_fault"
  | Induced_warning -> "induced_warning"
  | Add -> "add"
  | Delete -> "delete"
  | Time_begin -> "time_begin"
  | Time_end -> "time_end"
  | Unmatched -> "unmatched"*)

(*
let green, yellow, red, gray, brown, blue =
  "#11DD11", "yellow", "red", "gray", "brown", "blue";;

let color_of_status s =
  match s with
    Error -> red
  | Warning -> yellow
  | Dependency_error -> brown
  | Good -> green
  | Don't_care -> blue
  | Not_our_fault -> brown
  | Induced_warning -> brown
  | Add -> green
  | Delete -> red
  | Time_begin -> blue
  | Time_end -> blue
  | Unmatched -> gray
*)
(*let color_of_result r =
  match r with
    Failed -> red
  | Unsatisfied_dependencies -> brown
  | Warned -> green
  | Known_error -> blue
  | Probably_ok -> green
  | Perfect -> green
  | Information -> blue
  | Unknown -> gray
*)
(*let nstring_of_status s =
  match s with
    Error -> "Error"
  | Warning -> "Warning"
  | Dependency_error -> "Dependency error"
  | Good -> "Good"
  | Don't_care -> "Don't care"
  | Not_our_fault -> "Not our fault"
  | Induced_warning -> "Induced warning"
  | Add -> "Add"
  | Delete -> "Delete"
  | Time_begin -> "Time begin"
  | Time_end -> "Time end"
  | Unmatched -> "Unmatched"

let nresult_of_status s =
  match s with
    Error -> "Error"
  | Warning -> "Warning"
  | Dependency_error -> "Dependency error"
  | Good -> "Good"
  | Don't_care -> "Don't care"
  | Not_our_fault -> "Not our fault"
  | Induced_warning -> "Induced warning"
  | Add -> "Add"
  | Delete -> "Delete"
  | Time_begin -> "Time begin"
  | Time_end -> "Time end"
  | Unmatched -> "Unmatched"
*)
let iter_pair f lst =
  List.iter
    (fun (x,y) -> f x y)
    lst
;;

let build_module_page () = ()
let build_module_date_page () = ()
(*
type color = [`green | `yellow | `red | `gray | `brown | `blue]
let render_color c =
  match c with
    `green -> green
  | `yellow -> yellow
  | `red -> red
  | `gray -> gray
  | `brown -> brown
  | `blue -> blue
;;
let contrast c =
  match c with
    `green -> "black"
  | `yellow -> "black"
  | `red -> "white"
  | `gray -> "black"
  | `brown -> "white"
  | `blue -> "white"
let logcolor_class c =
  match c with
    `green -> "green"
  | `yellow -> "yellow"
  | `red -> "red"
  | `gray -> "gray"
  | `brown -> "brown"
  | `blue -> "blue"
*)
let logfile_missing = Unknown, "no log";;

let fn_cat lst = List.fold_left Filename.concat "" lst;;


let handlers : (string, cb:Parsers.callbacks -> string -> result) Hashtbl.t = Hashtbl.create 16;;
(*iter_pair
  (Hashtbl.add handlers)
  [
    "timing", Parsers.parse_timing;
  ];;
*)
exception PCDATA;;
begin try
  let rule_hash = Hashtbl.create 16 in
  let get_element node = match node with Xml.Element x -> x | Xml.PCData _ -> raise PCDATA in
  (*  iter_pair*)
  begin
    let rules = Xml.parse_file rules_filename in
    let tag_name, attributes, children = get_element rules in
    (*      error_endline tag_name;*)
    List.iter
      (fun logtype ->
	let tag_name, attributes, children = get_element logtype in
	(*	  error_endline tag_name;*)
	let extension = snd (List.find (fun (name, value) -> name = "extension") attributes) in
(*	prdbg_endline ("Processing " ^ extension);*)
	(Hashtbl.add handlers)
	  (extension)
	  begin
	    let prepend =
	      if (List.exists (fun (name, value) -> name = "prepend_rules") attributes) then begin
(*		if List.length children > 0 then
		  error_endline ("Both children and rules reference (using reference)!");*)
		let reference = (snd (List.find (fun (name, value) -> name = "prepend_rules") attributes)) in
		(*prdbg_endline ("Using " ^ reference ^ " for " ^ extension);*)
		Hashtbl.find rule_hash reference
	      end else []
	    in
	    let rule_list = 
	      List.flatten
		(List.map
		  (fun rule ->
		    let tag_name, attributes, children = get_element rule in
		    (*	      error_endline tag_name;*)
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
(*				  prdbg_endline ("Keeping fragment '" ^ frag ^ "'.");*)
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
(*		    | "Good" -> Good*)
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

(*    let hours, minutes, seconds =
      let timings =
	List.flatten
	(List.map
	   (fun (m, lst)
	     -> (*m,*)
	       (List.map
		  (fun x -> match x with None -> assert false | Some x -> x
		  ) (List.filter
		       (fun x -> match x with None -> false | Some x -> true
		       ) (List.map
			    (fun (a, ((b : result), c, d, e, _)) ->
			      e) lst)
		    )
	       )
	   ) out);
      in
      let bt =
	let bt = List.fold_left (+.) 0. timings in
	(int_of_float bt)
      in
      let hours = bt / (60 * 60) in
	let minutes = (bt - (hours * 60 * 60)) / 60 in
	let seconds = (bt - (hours * 60 * 60) - (minutes * 60)) in
	hours, minutes, seconds
    in*)
(*      let file_statistics =
        let sum =
          List.fold_left
            (fun c (_, _, s) ->
              match s with
                Error -> { c with error = c.error + 1 }
              | Warning -> { c with warning = c.warning + 1 }
              | Dependency_error -> { c with dependency_error = c.dependency_error + 1 }
              | Good -> { c with good = c.good + 1 }
              | Don't_care -> { c with don't_care = c.don't_care + 1 }
              | Not_our_fault -> { c with not_our_fault = c.not_our_fault + 1 }
              | Induced_warning -> { c with induced_warning = c.induced_warning + 1 }
              | Add -> { c with add = c.add + 1 }
              | Delete -> { c with delete = c.delete + 1 }
              | Time_begin -> { c with time_begin = c.time_begin + 1 }
              | Time_end -> { c with time_end = c.time_end + 1 }
              | Unmatched -> { c with unmatched = c.unmatched + 1 }
            )
            {
             error = 0;
             warning = 0;
             dependency_error = 0;
             good = 0;
             don't_care = 0;
             not_our_fault = 0;
             induced_warning = 0;
             add = 0;
             delete = 0;
             time_begin = 0;
             time_end = 0;
             unmatched = 0;
           }
            processed_lines
        in
	sum
(*        let first = ref true in
        List.fold_left (fun a b ->
          if String.length b = 0 then
            a
          else if !first then
            (first := false; (a ^ b))
          else
            (a ^ ", " ^ b)) ""
          [
           if sum.error > 0 then Printf.sprintf "E%i" sum.error else "";
           if sum.warning > 0 then Printf.sprintf "W%i" sum.warning else "";
           if sum.dependency_error > 0 then Printf.sprintf "D%i" sum.dependency_error else "";
(*            if sum.good > 0 then Printf.sprintf "G%i" sum.Good else ""; *)
(*            if sum.don't_care > 0 then Printf.sprintf "D%i" sum.Don't_care else ""; *)
           if sum.not_our_fault > 0 then Printf.sprintf "N%i" sum.not_our_fault else "";
           if sum.induced_warning > 0 then Printf.sprintf "I%i" sum.induced_warning else "";
           if sum.add > 0 then Printf.sprintf "+%i" sum.add else "";
           if sum.delete > 0 then Printf.sprintf "-%i" sum.delete else "";
(*            if sum.time_begin > 0 then Printf.sprintf "T%i" sum.Time_begin else ""; *)
(*            if sum.time_end > 0 then Printf.sprintf "T%i" sum.Time_end else ""; *)
           if sum.unmatched > 0 then Printf.sprintf "?%i" sum.unmatched else "";
         ]*)
      in
*)

let specialized_information t input_lines =
  match t with
      "timing" ->
        if List.length input_lines > 1 then
          ( let imperative_timing, timing = ref None, ref 0.0 in
              List.iter
                (fun (_, i) ->
                   try
                   Scanf.sscanf i "BUILD %s %s %i-%i-%i %i:%i:%i"
                     (fun state target year mon mday hour min sec ->
	                let (t, _) = Unix.mktime {
	                  Unix.tm_sec = sec;
   	                  Unix.tm_min = min;
   	                  Unix.tm_hour = hour;
   	                  Unix.tm_mday = mday;
   	                  Unix.tm_mon = mon;
   	                  Unix.tm_year = year - 1900;
	                  
   	                  Unix.tm_wday = 0;
   	                  Unix.tm_yday = 0;
   	                  Unix.tm_isdst = false;
	                } in
	                  match state with
	                      "BEGIN" ->
	                        imperative_timing := Some t
	                    | "END" ->
	                        (match !imperative_timing with
	                             Some ot ->
		                       let tdiff = t -. ot in
		                         timing := tdiff;
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
	    (* 		  let scanbuf = Lexing.from_channel (open_in (fn_cat [build_logs_name; date_name; file])) in *)
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
(*     prdbg_endline target_file;*)
       RenderCommon.html_head ~ch:o ~file:target_file ~title:t;
       Printf.fprintf o (*"<html><head><title>%s</title><link rel=\"stylesheet\" href=\"trurl_logs.css\" type=\"text/css\"></head>\n"*) "<div class=\"other\">";
     let in_idx s =
        match s with
(*type result = Perfect | Error | Dependency_error | Warning | Information | Unknown*)
          Error | Warning | Dependency_error -> true
        (*| Perfect*) | Information  -> false
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
           ) ["timing"; (*"diff";*) "autogen_sh"; "configure"; "make_all"; "make_check"; "make_install"];
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
      Printf.fprintf o "</div>" (*"</body></html>\n"*);
      RenderCommon.html_foot ~valid:false o;
      close_out o;

      let x =
        let lst =
          List.map
            (fun (_, _, s) ->
	      s
(*              match s with
                Error -> Failed
              | Warning -> Warned
              | Dependency_error -> Unsatisfied_dependencies
              | Good -> Perfect
              | Don't_care -> Information
              | Not_our_fault -> Known_error
              | Induced_warning -> Known_error
              | Add -> Information
              | Delete -> Information
              | Time_begin -> Information
              | Time_end -> Information
              | Unmatched -> Unknown*)
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
                | "cvs", "EXIT", 1 -> AlertSort.highest_alert !rval Warning (*type result = Perfect | Error | Dependency_error | Warning | Information | Unknown*)
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
  (fun dir_name ->
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
             l_logs = data; }
             (*	(List.map
	        (fun (step, (result, log_filename, (*float,*) lines)) ->
	        )) data;*)
        ) !output;
      in
    (glance : logs (*(string * result * logs) list*))
  )
let process_logdir = generate_logdir

let acc_logdata () (*: (string option * ((string * result * logs) list) list) list*) =
  let build_logs_name = "build_logs" in
(*  let build_logs_dir = Unix.opendir build_logs_name in*)
  let file_acc = Hashtbl.create 16 in

  let rec walkndirs dirname maxdepth n leaf_function =
    let build_logs_dir = Unix.opendir (Filename.concat build_logs_name dirname) in
    (try
       while true do
         let date_name = Unix.readdir build_logs_dir in
           match date_name with
               "." | ".." -> ()
             | entry ->
                 (*Printf.printf "entry: %i %s\n" n entry;*)
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
         (*Printf.printf "leaf: %s\n" name;*)
	 let build = None in
	   if Hashtbl.mem file_acc build then
	     Hashtbl.replace file_acc build
	       (name :: Hashtbl.find file_acc build)
	   else
	     Hashtbl.add file_acc build [name]
      );
(*
  (try
    while true do
      let date_name = Unix.readdir build_logs_dir in
      try
	Scanf.sscanf date_name "%_i-%_i-%_[0123456789]%_[0123456789]_%_[0123456789]%_[0123456789]_%s"
	  (fun build ->
	    let build = Some build in
	    if Hashtbl.mem file_acc build then
	      Hashtbl.replace file_acc build
		(date_name :: Hashtbl.find file_acc build)
	    else
	      Hashtbl.add file_acc build [date_name]
	  )
      with
	Scanf.Scan_failure _
      | Failure _
      | End_of_file ->
	  try
	    Scanf.sscanf date_name "%i-%i-%i"
	      (fun y m d ->
		let build = None in
		if Hashtbl.mem file_acc build then
		  Hashtbl.replace file_acc build
		    (date_name :: Hashtbl.find file_acc build)
		else
		  Hashtbl.add file_acc build [date_name]
	      )
	  with
	    Scanf.Scan_failure _
	  | Failure _
	  | End_of_file ->
	      ()
    done
  with
      End_of_file -> ());
*)

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
        (fun x ->
           (process_logdir (Filename.concat build_logs_name x) (*: logs*)(*(string * result * logs) list*))
        ) file_acc
  in
  let out_acc = ref [] in
  Hashtbl.iter
    (fun key data ->
      out_acc := (key, (process_acc data (*: ((string * result * logs) list) list*))) :: !out_acc
    ) file_acc;
  !out_acc
;;

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
(*  match acc with
      Some _ -> acc
    | None ->*)
        match find_special logs with
            None -> None
          | Some Timing x -> Some x
;;

let main () =
(*  let current_future =
    try
      let ch = open_in "global.state" in
      let future =
        try
          if (input_line ch) = "build" then
            Some (input_line ch)
          else None
        with End_of_file ->
          None
      in close_in ch; future
    with Sys_error _ -> None
  in
    (match current_future with
         None -> ()
       | Some future -> () (*print_string (future ^ "\n")*));*)
  let logs = acc_logdata () in
  let logs (*: (string option * ((string * result * logs) list) list) list*) = logs in

  (* Magical transformation - FROM list of [host 1-* module] TO list of [module 1-* host]. *)
(*  error_endline "Debug dump:";*)

  let get_logs () =
    let transform_target = Hashtbl.create 128 in
    let transform_target_future = Hashtbl.create 128 in
    List.iter
      (fun (rkey, logs) ->
	let platform = (match rkey with None -> "linux" | Some x -> x) in
	List.iter (* build iterations *)
	  (fun (build_results) ->
	    List.iter
	      (fun { l_module = project; l_result = result; l_logs = logs } ->
(*                 print_string (project ^ "\n");*)
(*                 List.iter (fun *)
		(* Get or create module entry. *)
		let project_hash =
                  match find_timing logs with
                      Some _ ->
		        if not (Hashtbl.mem transform_target project) then
		          Hashtbl.add transform_target project (Hashtbl.create 128);
		        Hashtbl.find transform_target project
                    | None ->
		        if not (Hashtbl.mem transform_target_future project) then
		          Hashtbl.add transform_target_future project (Hashtbl.create 128);
		        Hashtbl.find transform_target_future project                        
		in
		(* If platform has a build entry, ignore otherwise add this status (and a reference to the log object). *)
		if not (Hashtbl.mem project_hash platform) then
		  Hashtbl.add project_hash platform (result, logs);
(*		error_endline (" * " ^ platform ^ " -> " ^ project ^ " : " ^ (safe_string_of_result result));*)
	      ) build_results;
	  ) logs;
      ) logs;
    
    let projects = ref [] in
    let projects_future = ref [] in
    let aux transform_target projects =
    Hashtbl.iter
      (fun project project_hash ->
	let platforms = ref [] in
	let acc_result = ref Unknown in
        (*let acc_timing = ref None in*)
	Hashtbl.iter
	  (fun platform (result, logs) ->
             (*acc_timing := find_timing (!acc_timing) logs;*)
	    platforms := { tp_platform = platform; tp_result = result; tp_builds = [ (* fixme *) { tb_host = "cat"; tb_time = (match find_timing logs with None -> 0.0 | Some x -> x); tb_start = Unix.gmtime 0.0; tb_result = result; tb_logs = logs; } ] (*tp_time = find_timing logs (* TIME FIXME *);*) (*tp_logs = logs*) } :: !platforms;
	    acc_result := AlertSort.highest_alert !acc_result result;
	  ) project_hash;
	projects := { tm_module = project; tm_result = !acc_result; tm_revision = CVS { vc_repository = "fixme"; vc_path = "fixme"; vc_branch = "fixme"; vc_time = (Unix.gmtime 0.0); }; tm_platforms = !platforms } :: !projects;
      ) transform_target;
    in
      aux transform_target projects;
      aux transform_target_future projects_future;
      (!projects, !projects_future);
  in

  let transformed_logs, future =
    get_logs ()
  in
  let snapshot_result logs = (List.fold_left AlertSort.highest_alert Unknown (List.map (fun { tm_result = result } -> result) logs)) in
  let snapshots =
    let current = { ts_snapshot = (1970, 01, 01, 001); ts_result = snapshot_result transformed_logs; ts_modules = transformed_logs } in
    let previous = current (* FIXME *) in
      [current; previous]
  in
  let future = match future with [] -> None | _ -> Some { ts_snapshot = (2100, 01, 01, 001); ts_result = snapshot_result future; ts_modules = future } in

  Render.project_pages snapshots;

  Render.frontpage snapshots future;

  (* TODO:
     - Load map
     - compare against current and make a changeset
     - Save map
     - fork() hooks to select state changes
  *)
  let old_results = Results.load () in
  let new_results, changeset =
    Results.merge old_results snapshots
  in
    Results.save new_results;
    Results.hooks changeset snapshots;
;;

main ()
