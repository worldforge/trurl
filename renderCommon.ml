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

open Types;;
open Config;;
open ExtLib;;
open Util;;

let up_to n lst =
  let rec aux lst acc nacc =
    if nacc >= n then
      acc
    else
      match lst with
          [] -> acc
        | hd :: tl ->
            aux tl (hd :: acc) (nacc + 1)
  in
    List.rev (aux lst [] 0)
;;

let sort_steps lst =
  List.sort
    lst
    ~cmp:(fun { f_step = a; } { f_step = b; } ->
      let res = match (a, b) with
      | "timing", _ -> -1
      | _, "timing" -> 1
      | "autogen_sh", _ -> -1
      | _, "autogen_sh" -> 1
      | "configure", _ -> -1
      | _, "configure" -> 1
      | "make_all", _ -> -1
      | _, "make_all" -> 1
      | "make_check", _ -> -1
      | _, "make_check" -> 1
      | "make_install", _ -> -1
      | _, "make_install" -> 1
      | _, _ -> compare a b
      in
(*      Printf.eprintf "%s > %s => %i\n" a b res;*)
      res
    )

let xml_escape_attribute = (* FIXME *)
  String.map (fun x -> match x with '+' -> '_' | _ -> x)

let string_of_snapshot (year, month, day, build) =
  let snapshot = Printf.sprintf "%04i.%02i.%02i-%03i" year month day build in
    Printf.sprintf "<span class=\"snapshot\"><a href=\"snapshot-%s.html\">%s</a></span>" snapshot snapshot
;;

let snapshot_safe_string (year, month, day, build) =
  Printf.sprintf "%04i.%02i.%02i-%03i" year month day build
;;

let cached_cvs_diff ~name ~current ~previous : string list =
  let orig_wd = Sys.getcwd () in
  let root = "/home/trurl/work" in
  let repo = current.vc_repository in
  let prefix = current.vc_path in
  let executable = "cvs" in
  let arguments = [|"diff";"-u";"-D";tm_to_cvs_iso8601 previous;"-D";tm_to_cvs_iso8601 current.vc_time|] in
  let command = Array.fold_left (fun acc str -> acc ^ " \"" ^ str ^ "\"") "cvs" arguments in
  let calc_source_dir type_str repo prefix = Printf.sprintf "%s/source/%s/%s/%s%s" root type_str (md5 repo) prefix name in
  let cache_dir = root ^ "/cache/cvs-diff/" ^ (md5 repo) in
  let cache_file = cache_dir ^ "/" ^ (md5 (Printf.sprintf "%s/%s %s" prefix name command)) in
    (* cache_dir/repo_md5/(prefix/name-cmd)_md5 *)
  let diff =
    if Sys.file_exists cache_file then
      let ch = open_in_bin cache_file in
      let diff = Std.input_all ch in
	close_in ch;
	diff
    else
      begin
	let wd = calc_source_dir "cvs" repo (if current.vc_path <> "" then current.vc_path ^ "/" else "") in
	let buffer = Buffer.create 128 in
	let stdout = Shell.to_buffer buffer in
	  (try
	     ignore (Sys.command ("mkdir -p " ^ cache_dir));
	     Sys.chdir wd;
	     (try
		Shell.call ~ignore_error_code:true (* FIXME, cvs diff returns non-zero if differences are found OR an error occured, we'll just have to hope it goes well *) ~stdout ~stderr:stdout [Shell.command ~arguments (Shell_sys.lookup_executable executable)]
	      with Shell.Subprocess_error _ -> ());
	     Sys.chdir orig_wd;
	   with e ->
	     Sys.chdir orig_wd;
	     prerr_endline ("Failed command: " ^ command);
	     prerr_endline ("pwd: " ^ wd);
	     raise e);
	  let diff = Buffer.contents buffer in
	  let cache_file_tmp = cache_file ^ ".tmp" in
	  let ch = open_out_bin (cache_file_tmp) in
	    output_string ch diff;
	    close_out ch;
	    Sys.rename cache_file_tmp cache_file;
	    diff
      end
  in
  let lines = (Pcre.split ~pat:"\n" diff) in
    List.filter
      (fun str ->
	 not (str = "" || begins_with "cvs diff: Diffing " str)
      ) lines
;;

let same_revision name a b =
  match a, b with
    | CVS { vc_time = previous; }, (CVS current) ->
	(cached_cvs_diff ~name ~current ~previous) = []
    | _ -> a = b
;;

let doctype_strict = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">";;
let doctype_loose = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">";;
let doctype_xhtml11 = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">";; (* TODO: UTF-8, remember the http headers. *)

let meta_refresh_interval = 10;;
let meta_refresh = if enable_debug_features then Printf.sprintf "<meta http-equiv=\"refresh\" content=\"%i\" />\n" meta_refresh_interval else "";;

let escape_html =
  Pcre.substitute
    ~rex:(Pcre.regexp "[<>&\"]")
    ~subst:(function "<" -> "&lt;" | ">" -> "&gt;" | "&" -> "&amp;" | "\"" -> "&quot;" | x -> x)
;;

let mklink link name =
  "<a href=\"" ^ link ^ "\">" ^ (escape_html name) ^ "</a>"

let html_head ?file ?(dynamic="") ?title ch =
  Printf.fprintf ch "%s\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n%s<title>%sWorldForge's Autobuilder</title>\n<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\" />\n<link rel=\"stylesheet\" href=\"/trurl/static/trurl_shared.css\" type=\"text/css\" />\n<link rel=\"stylesheet\" href=\"/trurl/static/trurl_frontpage.css\" type=\"text/css\" />\n</head>\n<body>\n<div class=\"top_menu\">%s<!--<ul>%s</ul>-->%s</div>%s<div class=\"sub_menu\">%s</div>\n"
    ((if match file with None -> false | Some file -> ends_with ".php" file then "<?php echo '<?xml version=\"1.0\" encoding=\"utf-8\" ?>'; ?>" else "<?xml version=\"1.0\" encoding=\"utf-8\" ?>") ^ "\n" ^ doctype_xhtml11)
    meta_refresh (match title with None -> "" | Some t -> t ^ " - ")
    (if enable_debug_features then "Debug mode activated. This page will refresh every " ^ (if meta_refresh_interval = 10 then "ten" else Printf.sprintf "%i" meta_refresh_interval) ^ " seconds." else "")
    (List.fold_left
       (fun str (link, ext, title) ->
          str ^ "<li><a href=\"" ^ link ^ "\">" ^ title ^ (if ext then " <img src=\"/trurl/static/images/external.png\" alt=\"external link\" />" else "") ^ "</a></li>"
       ) "" (
      [
	("http://www.worldforge.org/", true, "WorldForge");
	("http://yellow.worldforge.org/", true, "Yellow");
	("/trurl/", false, "Trurl Frontpage");
      ] @ (if enable_debug_features then
	[
	  ("#", false, "(Modules)");
	  ("#", false, "(Platforms)");
	  ("#", false, "(Builds In Progress)");
	] else [])))
    ("<a href=\"http://www.worldforge.org/\">WorldForge</a>'s <a href=\"" ^ html_root ^ "\">Autobuilder</a>")
    dynamic
    (List.fold_left
       (fun acc (uri, text) ->
	  let is_self = match file with None -> false | Some file -> file = uri || (begins_with "index." file && uri = "") in
	  let pre, post =
	    if not is_self then
	      ("<a href=\"" ^ html_root ^ uri ^ "\">"), "</a>"
	    else "<b>", "</b>"
	  in
	    acc ^ " " ^ pre ^ text ^ post
       ) "" ["", "Frontpage"; "timeline.html", "Timeline"; "snapshots.html", "Snapshot Table"])
;;

let html_foot ?(valid=true) ch =
  (if valid then
     Printf.fprintf ch "<p>\n<a href=\"http://validator.w3.org/check?uri=referer\">\n<img style=\"border:0;width:88px;height:31px\" src=\"static/images/valid-xhtml11\" alt=\"Valid XHTML 1.1\" />\n</a>\n<a href=\"http://jigsaw.w3.org/css-validator/\">\n<img style=\"border:0;width:88px;height:31px\" src=\"static/images/vcss\" alt=\"Valid CSS!\" />\n</a>\n</p>\n");
  Printf.fprintf ch "</body>\n</html>\n";
;;

let project_url project = "module_" ^ project ^ ".html";;
let href_of_project project =
  "<a href=\"" ^ project_url project ^ "\">" ^ project ^ "</a>"
;;

let fmt' = Util.time_string;;

let time_as_gears time =
  (if time = None then "<img src=\"static/images/gears.png\" alt=\"building\" />" else "")

let time_as_gears_or_time (time : float option) =
  match time with
      Some seconds -> "(" ^ (fmt' seconds) ^ ")"
    | None -> time_as_gears time

let split s c =
  try
    let i = String.index s c in
      ((String.sub s 0 i), (String.sub s (i + 1) (String.length s - i - 1)))
  with Not_found ->
    (s, "")

let build_html =
  (fun { tb_host = tb_host; tb_time = tb_time; tb_start = tb_start; tb_result = tb_result; tb_logs = tb_logs; } ->
     
     let buffer = Buffer.create 1024 in
     let p s = Buffer.add_string buffer s in
     let pr title description = p (Printf.sprintf "<dt>%s</dt>\n<dd>%s</dd>\n" title description) in
     let dl result = p (Printf.sprintf "<dl class=\"%s\">" (safe_string_of_result result)) in
     let dl' () = p "</dl>" in

       dl tb_result;
       pr "Host" tb_host;
       pr "Start" (tm_to_string tb_start);
       pr "Time" (fmt' tb_time);
       pr "Result" (safe_string_of_result tb_result);
     
       p "<dt>Logs</dt>";
       List.iter
	 (fun { f_step = step; f_filename = filename } -> 
            p (Printf.sprintf "<dd><a href=\"generated/%s\">%s</a></dd>" filename step);
	 ) tb_logs;
       
       dl' ();
       Buffer.contents buffer
  )
;;
let snapshot_module_platform_to_render = ref [];;
let snapshot_module_platform_href ({ tp_platform = tp_platform; tp_module = tp_module; tp_snapshot = tp_snapshot; tp_builds = tp_builds; } as platform) =
  let file = "snapshot_module_platform-" ^ (snapshot_safe_string tp_snapshot) ^ "_" ^ tp_module ^ "_" ^ tp_platform ^ ".html" in
    snapshot_module_platform_to_render := (file, platform) :: (!snapshot_module_platform_to_render);
    file
;;
let render_platform platform =
  let (platform', version) = split platform.tp_platform '-' in
    "<a href=\"" ^ (snapshot_module_platform_href platform) ^ "\"><img src=\"static/images/platforms/" ^ platform' ^ platform_image_version ^ ".png\" alt=\"" ^ platform' ^ "\" /> " ^ version ^ "</a>"
;;
let render_platform' platform =
  let (platform', version) = split platform '-' in
    "<img src=\"static/images/platforms/" ^ platform' ^ platform_image_version ^ ".png\" alt=\"" ^ platform' ^ "\" /> " ^ version ^ ""
;;

let extract_error_lines { tp_builds = tp_builds; tp_result = tp_result; } =
  let platform_logs = (List.hd tp_builds).tb_logs in
  let { f_step = step; f_result = step_result; f_filename = log_filename; f_sections = f_sections; } =
    List.find (fun { f_result = step_result; } -> tp_result = step_result) (sort_steps platform_logs)
  in
  let error_messages = (List.filter (fun { s_result = s_result; } -> s_result = tp_result) f_sections) in
    error_messages
;;

let lines_equal_ignore_numbers a b =
  (List.length a = List.length b) &&
  List.fold_left2
    (fun acc a b ->
       (a.s_result = b.s_result) &&
	 (List.length a.s_lines = List.length b.s_lines) &&
	 List.fold_left2
	 (fun acc (_, aline, ares, _) (_, bline, bres, _) ->
	    acc && (ares = bres && aline = bline)
	 ) true a.s_lines b.s_lines
    ) true a b
;;

let merge_platform_errors lst =
  let rec merge acc lst =
    match lst with
	[] -> List.rev acc
      | hd :: tl ->
	  let equal, tl' =
	    List.partition
	      (fun other_platform ->
		 lines_equal_ignore_numbers (extract_error_lines hd) (extract_error_lines other_platform)
	      ) tl
	  in
	    merge ((hd, equal) :: acc) tl'
  in
    merge [] lst
;;

let render_mistakes platforms_mistakes =
  "<ul>\n"
  ^ (List.fold_left
       (fun str ({ tp_platform = platform; tp_result = platform_result; (*tp_logs = platform_logs; tp_time = time;*) tp_builds = tp_builds } as platform', equal) ->
          let platform_logs = (List.hd tp_builds).tb_logs in
	    str (* folded string *)
	    ^ "<li class=\"" ^ (safe_string_of_result platform_result) ^ "\">\n"
	    ^ (render_platform platform') ^ (List.fold_left (fun acc platform -> acc ^ " " ^ (render_platform platform)) "" equal) (* FIXME, would be nice to able able to access the other logs as well *) ^ "\n" ^
	      begin (* TODO: How do we give extensible knowledge of build-step sorting? *)
		let { f_step = step; f_result = step_result; f_filename = log_filename; f_sections = f_sections; } =
		  try
		    List.find (fun { f_result = step_result; } -> platform_result = step_result) (sort_steps platform_logs)
		  with Not_found -> (* FIXME, only happens when we're hiding actual state *)
		    let platform_result = (List.hd tp_builds).tb_result in
		      List.find (fun { f_result = step_result; } -> platform_result = step_result) (sort_steps platform_logs)
		in
		let error_messages = (List.filter (fun { s_result = s_result; } -> s_result = platform_result) f_sections) in
		  (mklink ("generated/" ^ log_filename) step) ^ ":" ^
		    if List.length error_messages > 0 then
		      "<br />\n" ^
			"<tt>" ^
			List.fold_left
			(fun str { s_lines = s_lines; } -> str ^
			   (List.fold_left
			      (fun str' (line_no, line_data, line_result, _) -> str' ^
				 (Printf.sprintf (*"<dl>\n<dt>%i</dt><dd><tt>%s</tt></dd>\n</dl>\n"*) "%i: %s<br/>\n" line_no (escape_html line_data))
			      ) "" s_lines)
			) "" (up_to 10 error_messages) ^
			"</tt>" ^
			(if List.length error_messages > 10 then
			   Printf.sprintf "<tt>Log contains %i more messages.</tt><br/>\n" (List.length error_messages - 10)
			 else "")
		    else (* Meta told us something bad happened. *)
		      " Non-zero exit status.<br />Check meta for specifics."
	      end
	    ^ "</li>\n") "" (merge_platform_errors platforms_mistakes))
  ^ "</ul>\n"
;;

let snapshot_module_platform_href_do_render (file, ({ tp_platform = tp_platform; tp_module = tp_module; tp_snapshot = tp_snapshot; tp_builds = tp_builds; } as platform)) =
  let file_full = (Filename.concat target_dir file) in
(*FIXME    if (not (Sys.file_exists file_full)) ||
      (let stat_target = Unix.stat file_full
       in stat_target.Unix.st_mtime < stat_self.Unix.st_mtime)
    then*)
      begin
	let ch = open_out file_full in
	let p = output_string ch in
	  html_head ~title:(tp_module ^ " " ^ tp_platform ^ " " ^ (snapshot_safe_string tp_snapshot)) ch;
	  p ("<h1>" ^ (tp_module ^ " " ^ tp_platform ^ " " ^ (snapshot_safe_string tp_snapshot)) ^ "</h1>");
	  p (render_mistakes [platform]);
	  List.iter (fun build -> p (build_html build)) tp_builds;
	  html_foot ch;
	  close_out ch;
      end;
;;

let render_platform_gears platforms =
  (List.fold_left (fun str { tp_platform = platform; tp_result = result; (*tp_time = time; tp_logs = logs*) } -> str (*^ (time_as_gears time)*)) "" platforms)
;;

let acc_snapshot (arrlen, hash) ({ ts_snapshot = (year, month, day, build); } as snapshot) past future =
  List.iter
    (fun module_ ->
       let arr =
	 try
	   Hashtbl.find hash module_.tm_module
	 with Not_found ->
	   let arr = Array.make arrlen None in
	     Hashtbl.add hash module_.tm_module arr;
	     arr
       in
	 Array.set arr (List.length past) (Some module_)
    ) snapshot.ts_modules;
;;

let calc_module_hash logs =
  let module_hash =
    (List.length logs, Hashtbl.create 32)
  in
  let rec part past future =
    match past with
	hd :: tl ->
	  (fun ({ ts_snapshot = (year, month, day, build); } as snapshot) ->
(*	     let snapshot_name = Printf.sprintf "%04i.%02i.%02i-%03i" year month day build in*)
	     (*let snapshot_channel = open_out (Filename.concat target_dir ("snapshot-" ^ snapshot_name ^ ".html")) in*)
	       (*render_snapshot snapshot_channel snapshot (List.rev tl) future;*)
	       acc_snapshot module_hash snapshot (List.rev tl) future;
	       (*close_out snapshot_channel;*)
	  ) hd;
	  part tl (hd :: future)
      | [] -> ()
  in
    part logs [];
    snd module_hash
;;

let git_href repo branch commit text =
  let res = (Pcre.exec ~rex:(Pcre.regexp "^git://(.*)/(.*\\.git)$") repo) in
  let httprepo = "http://" ^ (Pcre.get_substring res 1) ^ "/?p=" ^ (Pcre.get_substring res 2) in
  "<a href=\"" ^ httprepo ^
    (match commit with
	 None -> ";a=shortlog" ^ ";h=refs/heads/" ^ branch
       | Some commit -> ";a=commitdiff;h=" ^ commit) ^
    "\">" ^ text ^ "</a>"
;;
let href_git_commitdiff repo ~current ~previous =
  let res = (Pcre.exec ~rex:(Pcre.regexp "^git://(.*)/(.*\\.git)$") repo) in
  let httprepo = "http://" ^ (Pcre.get_substring res 1) ^ "/?p=" ^ (Pcre.get_substring res 2) in
  let name = (Pcre.get_substring res 2) in
  let name = String.sub name 0 (String.length name - 4) (* cut off .git *) in
(* $ git diff --shortstat
    6 files changed, 201 insertions(+), 12 deletions(-) *)
    if current = previous then
      ""
    else
      let text =
	let orig_wd = Sys.getcwd () in
	let cvs_root = "/home/trurl/work" in
	let calc_source_dir type_str repo prefix = Printf.sprintf "%s/source/%s/%s/%s%s" cvs_root type_str (md5 repo) prefix name in
	let wd = calc_source_dir "git" repo "" in
	let buffer = Buffer.create 128 in
	let stdout = Shell.to_buffer buffer in
	  (try
	     Sys.chdir wd;
	     Shell.call ~stdout [Shell.command ~arguments:[|"--shortstat";previous;current|] (Shell_sys.lookup_executable "git-diff")];
	     Sys.chdir orig_wd; 
	   with e ->
	     Sys.chdir orig_wd; 
	     raise e);
       let stats = Buffer.contents buffer in
	    try
	      let res = (Pcre.exec ~rex:(Pcre.regexp "^\\s*(\\d+) files changed, (\\d+) insertions\\(\\+\\), (\\d+) deletions\\(-\\)\\s*$") stats) in
		(*(Pcre.get_substring res 1) ^ "f: +" ^ (Pcre.get_substring res 2) ^ " -" ^ (Pcre.get_substring res 3)*)
		"+" ^ (Pcre.get_substring res 2) ^ " -" ^ (Pcre.get_substring res 3)
	    with Not_found -> "Internal Error"
	      (* FIXME, calc lines *)
      in
	"<a class=\"diff\" href=\"" ^ httprepo ^ ";a=commitdiff;h=" ^ current ^ ";hp=" ^ previous ^
	"\">" ^ text ^ "</a>"
;;  
let href href text =
  "<a href=\"" ^ href ^ "\">" ^ text ^ "</a>"

let href_module = href_of_project;;
let href_diff ~previous ~current =
  let prev = previous in
  let module_ = current in
    match prev.tm_revision, module_.tm_revision with
	Git { (* FIXME, verify vg_repository = prev_repository;*) (*vg_branch = : string; FIXME, verify branches, if they differ it's possibly a contract breach (have to ponder merges though*) vg_commit = prev_commit; }, Git { vg_repository = repo; (*vg_branch : string;*) vg_commit = curr_commit; } ->
	  (href_git_commitdiff repo ~current:curr_commit ~previous:prev_commit);
      | CVS { vc_time = previous; }, (CVS current) ->
	  let diff = (cached_cvs_diff ~name:module_.tm_module ~current ~previous) in
	  let diff_file = "diff_" ^ module_.tm_module ^ "_" ^ (md5 ((tm_to_iso8601_utc previous) ^ " diff " ^ (tm_to_iso8601_utc current.vc_time))) ^ ".html" in
	  let diff_file_full = (Filename.concat target_dir diff_file) in
	    if List.length diff > 0 then
	      begin
 		if (not (Sys.file_exists diff_file_full)) ||
		  (let stat_target, stat_self =
		     Unix.stat diff_file_full, stat_self
		   in stat_target.Unix.st_mtime < stat_self.Unix.st_mtime)
		then
		  begin let ch = open_out (Filename.concat target_dir diff_file) in
		  let p = output_string ch in
		    html_head ~title:"Diff" ch;
		    p "Diff for ";
		    p module_.tm_module;
		    p " from ";
		    p (tm_to_iso8601_utc previous);
		    p " to ";
		    p (tm_to_iso8601_utc current.vc_time);
		    p "<br/>\n";
		    List.iter
		      (fun line ->
			 (* line numbers? *)
			 p "<tt";
			 (if begins_with "+" line then
			    p " class=\"insertion\""
			  else if begins_with "-" line then
			    p " class=\"deletion\""
			  else ());
			 p ">";
			 p (ExtString.String.replace_chars (fun c -> match c with ' ' -> "&nbsp;" | c -> ExtString.String.of_char c) (escape_html line));
			 p "</tt>";
			 p "<br/>\n";
		      ) diff;
		    html_foot ch;
		    close_out ch;
		  end;
		let ins = List.fold_left (fun acc line -> if (begins_with "+" line) && not (begins_with "+++" line) then acc + 1 else acc) 0 diff in
		let del = List.fold_left (fun acc line -> if (begins_with "-" line) && not (begins_with "---" line) then acc + 1 else acc) 0 diff in
		  "<a href=\"" ^ diff_file ^ "\">+" ^ (string_of_int ins) ^ " -" ^ (string_of_int del) ^ "</a>"
	      end
	    else ""
      | _ -> "n/a" (* FIXME *)
;;

let equal_modules a b =
  a.tm_module = b.tm_module &&
  (same_revision a.tm_module a.tm_revision b.tm_revision) &&
  a.tm_result = b.tm_result (* FIXME *) &&
  (* ignore tm_result and verify that for all tm_platforms in a the result lines are equal to those in b and the other way around, if so, merge missing platforms into b and return b *)
  (List.length a.tm_platforms) = (List.length b.tm_platforms) (* is this correct? FIXME *) &&
  List.fold_left2
  (fun acc a' b' ->
     acc &&
       a'.tp_platform = b'.tp_platform &&
      a'.tp_result = b'.tp_result &&
      true (* FIXME tp_builds *)
  ) true a.tm_platforms b.tm_platforms
;;

let icon_and_text ?(small=false) result =
  let img, text =
    match result with
        Error -> "error", "Error"
      | Dependency_error -> "deperror", "Error (dependency)"
      | Warning -> "warning", "Warning"
      | Information -> "ok", "Success"
      | Unknown -> "unknown", "Internal error"
  in
    ("<img src=\"static/images/" ^ img ^ (if small then "-16x16" else "") ^ ".png\" alt=\"" ^ text ^ "\" />", text)
;;
let html_of_snapshot ?(current=false) snapshot =
  let img, text = icon_and_text snapshot.ts_result in
    (if current then "<div class=\"current\">" else "<div>") ^
      img ^ " <div>" ^ text ^ "<br />" ^ (string_of_snapshot snapshot.ts_snapshot) ^ "</div>" ^ "</div>"
;;

let module_history_merge arr =
  let merged = ref [] in
  Array.iteri
    (fun i mopt ->
       match mopt with
	   None -> ()
	 | Some mcurr ->
	     match !merged with
		 [] ->
		   merged := mcurr :: (!merged)
	       | hd :: tl ->
		   if equal_modules hd mcurr then
		     merged := mcurr :: tl
		   else
		     merged := mcurr :: (!merged)
    ) arr;
    Array.of_list (List.rev_map (fun x -> Some x) !merged)
;;

let module_history_to_table_row ?(merge=false) ?(always_arrows=false) ?(show_snapshot_id=false) ?title ?limit ?padding arr =
  let buffer = Buffer.create 1024 in
  let p s = Buffer.add_string buffer s in
  let titles, module_name =
    match title with
	None -> false, ""
      | Some title -> true, title
  in
  let arr = if merge then module_history_merge arr else arr in
  let arr =
    match limit with
	None -> arr
      | Some max ->
	  let len = Array.length arr in
	    if len > max then
	      Array.sub arr (len - max) max
	    else arr
  in
  let arr =
    match padding, limit with
	_, None
      | None, _ -> arr
      | Some side, Some max ->
	  let len = Array.length arr in
	    if len < max then
	      let arr' = Array.make max None in
		Array.blit arr 0 arr' (match side with `left -> (max - len) | `right -> 0) len;
		arr'
	    else arr
  in
  let columns = Array.length arr in
    p "<tr>";
    if titles then
      p ("<th>" ^ (href_module module_name) ^ "</th>");
    Array.iteri
      (fun i mopt ->
	 match mopt with
	     None ->
	       (*if not merge then*)
		 (p "<td class=\"unknown empty\"></td>";
		  (if (i+1) < columns then p "<td class=\"next\"></td>"));
	   | Some mcurr ->
	       let equal =
		 (if (i+1) < columns then
		    match arr.(i+1) with
			None -> false (*merge*) (*false*) (* If we merge, we already have the edge *)
		      | Some mnext ->
			  if (href_diff ~current:mnext ~previous:mcurr) (* FIXME *) = "" && (equal_modules mcurr mnext) then true else false
		  else false (*merge*) (*false*) (* same as above re: edge *)) &&
		   (if i > 0 then
		      match arr.(i-1) with
			  None -> true (* above we want to ensure we render the last one *)
			| Some mprev ->
			    if (href_diff ~current:mcurr ~previous:mprev) (* FIXME *) = "" && (equal_modules mprev mcurr) then true else false
		    else true)
	       in
		 if equal then
(*		   if merge then
		     ()
		   else*)
		     p ("<td class=\"" ^ (safe_string_of_result mcurr.tm_result) ^ " empty\"></td>")
		 else
		   begin
		     p ("<td class=\"" ^ (safe_string_of_result mcurr.tm_result) ^ "\">");
		     let e, d, w, s, u =
		       let rec acc e d w s u lst =
			 match lst with
			     [] -> (e, d, w, s, u)
			   | hd :: tl ->
			       match hd.tp_result with
				   Error -> acc (hd :: e) d w s u tl
				 | Dependency_error -> acc e (hd :: d) w s u tl
				 | Warning -> acc e d (hd :: w) s u tl
				 | Information -> acc e d w (hd :: s) u tl
				 | Unknown -> acc e d w s (hd :: u) tl
		       in acc [] [] [] [] [] mcurr.tm_platforms
		     in
		     let fold_platforms lst =
		       List.fold_left
			 (fun str p ->
			    str ^ " " ^ (render_platform p)
			 ) "" (List.sort lst)
		     in
		     let fold_platforms' lst =
		       List.fold_left
			 (fun str p ->
			    str ^ " " ^ (render_platform' p)
			 ) "" (List.sort lst)
		     in
		     let perhaps lst =
		       if List.length lst > 0 then
			 let result = (List.hd lst).tp_result in
			 let img, text = icon_and_text ~small:true result in
			   p (Printf.sprintf "<li class=\"%s\">%s:%s</li>" (safe_string_of_result result) img (* text takes too much space here *) (fold_platforms lst));
		     in
		     let perhaps' (action, action_safe) lst =
		       if List.length lst > 0 then
			 p (Printf.sprintf "<li class=\"%s\"><img src=\"static/images/%s-16x16.png\" alt=\"%s\" />:%s</li>" action_safe action_safe action (fold_platforms' lst));
		     in
		       p "<ul>";
		       if show_snapshot_id then
			 p ("<li class=\"snapshot\">" ^ (string_of_snapshot mcurr.tm_snapshot) ^ "</li>");
		       perhaps e;
		       perhaps d;
		       perhaps w;
		       perhaps s;
		       perhaps u;
		       perhaps' ("Building", "building") (List.map (fun { tmb_platform = platform } -> platform) mcurr.tm_building);
		       perhaps' ("Scheduled", "scheduled") (List.map (fun { tms_platform = platform } -> platform) mcurr.tm_scheduled);
		       p "</ul>";
		       p "</td>";
		   end;
      (*		 if merge && equal then
		   ()
		 else*)
		   (if (i+1) < columns then
		      match arr.(i+1) with
			  None -> p "<td class=\"next\"></td>"
			| Some mnext ->
			    let diff_text = (href_diff ~current:mnext ~previous:mcurr) in			       
			    let escalation = (match AlertSort.highest_alert_compare mcurr.tm_result mnext.tm_result with -1 -> " <img src=\"static/images/error-increase-16x16.png\" alt=\"Errorlevel escalated.\" />" | 1 -> " <img src=\"static/images/error-decrease-16x16.png\" alt=\"Errorlevel de-escalated.\" />" | 0 -> if always_arrows then " <img src=\"static/images/next-16x16.png\" alt=\"\" />" else "" | _ -> (prerr_endline "impossible result from highest_alert_compare"; "")) in
			      p ("<td class=\"next\">" ^ diff_text ^ escalation ^ "</td>")
		   );
      ) arr;
    if titles then
      p ("<th>" ^ (href_module module_name) ^ "</th>");
    p "</tr>";
    Buffer.contents buffer
;;
