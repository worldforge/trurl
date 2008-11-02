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
open RenderCommon

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
		"+" ^ (Pcre.get_substring res 2) ^ "/-" ^ (Pcre.get_substring res 3)
	    with Not_found -> "Internal Error"
	      (* FIXME, calc lines *)
      in
	"[<a class=\"diff\" href=\"" ^ httprepo ^ ";a=commitdiff;h=" ^ current ^ ";hp=" ^ previous ^
	"\">" ^ text ^ "</a>]"
;;  
let href href text =
  "<a href=\"" ^ href ^ "\">" ^ text ^ "</a>"
  
	
let dump_module { tm_module = project; tm_result = tm_result; tm_platforms = platforms; tm_revision = tm_revision; } ch =
  let pr title description = Printf.fprintf ch "<dt>%s</dt>\n<dd>%s</dd>\n" title description in
  let dl result = Printf.fprintf ch "<dl class=\"%s\">" (safe_string_of_result result) in
  let dl' () = Printf.fprintf ch "</dl>" in
    Printf.fprintf ch "<div class=\"%s\">" (safe_string_of_result tm_result);
    Printf.fprintf ch "<h1>%s</h1>" project;
    dl tm_result;
    Printf.fprintf ch "<dt>%s</dt>" "Revision";
    Printf.fprintf ch "<dd>%s</dd>"
      (match tm_revision with CVS cvs -> "CVS: " ^ cvs.vc_repository ^ " using path " ^ cvs.vc_path ^ " and branch " ^ cvs.vc_branch ^ " at time " ^ (tm_to_string cvs.vc_time)
	 | Subversion svn -> "SVN: " ^ svn.vs_repository ^ " using path " ^ svn.vs_path ^ " at r" ^ (string_of_int svn.vs_revision)
	 | Git git -> "Git: " ^ (href git.vg_repository git.vg_repository) ^ " using branch " ^ (git_href git.vg_repository git.vg_branch None git.vg_branch) ^ " at commit " ^ (git_href git.vg_repository git.vg_branch (Some git.vg_commit) git.vg_commit));
    pr "Result" (safe_string_of_result tm_result);
    Printf.fprintf ch "<dt>%s</dt>" "Revisions";
    List.iter
             (fun { tp_platform = tp_platform; tp_result = tp_result; tp_builds = tp_builds; } ->
                Printf.fprintf ch "<dd>";
                dl tp_result;
                pr "Platform" tp_platform;
                pr "Result" (safe_string_of_result tp_result);
                Printf.fprintf ch "<dt>%s</dt>" "Builds";
                List.iter
                  (fun { tb_host = tb_host; tb_time = tb_time; tb_start = tb_start; tb_result = tb_result; } ->
                Printf.fprintf ch "<dd>";
                     dl tb_result;
                     pr "Host" tb_host;
                     pr "Start" (tm_to_string tb_start);
                     pr "Time" (RenderCommon.fmt' tb_time);
                     pr "Result" (safe_string_of_result tb_result);
                Printf.fprintf ch "</dl>";
                Printf.fprintf ch "</dd>";           
                  ) tp_builds;
                Printf.fprintf ch "</dl>";
                Printf.fprintf ch "</dd>";           
             ) platforms;
    dl' ();
      Printf.fprintf ch "Build %s.%s" (safe_string_of_result tm_result) (if tm_result = Information then "<img src=\"static/images/star.png\" alt=\"star\" />" else "");

      List.iter
        (fun { tp_platform = platform; tp_builds = tp_builds } ->
           let logs = (List.hd tp_builds).tb_logs in
           Printf.fprintf ch "<dt>%s</dt>" platform;
           List.iter
             (fun { f_step = step; f_filename = filename } -> 
                Printf.fprintf ch "<dd><a href=\"generated/%s\">%s</a></dd>" filename step;
             ) logs
        ) platforms;

      Printf.fprintf ch "</div>";
;;

(* Project pages *)
let project_pages (logs : tr_logs) =
  List.iter
    (fun ({ tm_module = project; tm_result = tm_result; tm_platforms = tm_platforms; tm_revision = tm_revision; } as m) ->
      let ch = open_out (Filename.concat target_dir (project_url project)) in
      html_head ~file:(project_url project) ~title:project ch;
      Printf.fprintf ch "<div class=\"other\">";
      dump_module m ch;
      Printf.fprintf ch "</div>";
      html_foot ch;
      close_out ch;
    ) (List.hd logs).ts_modules
;;

let string_of_snapshot (year, month, day, build) =
  let snapshot = Printf.sprintf "%04i.%02i.%02i-%03i" year month day build in
    Printf.sprintf "<span class=\"snapshot\">[<a href=\"snapshot-%s.html\">%s</a>]</span>" snapshot snapshot
;;

let icon_and_text ?(small=false) result =
  let img, text =
    match result with
        Error -> "error", "Error"
      | Dependency_error -> "error", "Error (dependency)"
      | Warning -> "warning", "Warning"
      | Information -> "ok", "Success"
      | Unknown -> "error", "Internal error"
  in
    ("<img src=\"static/images/" ^ img ^ (if small then "-16x16" else "") ^ ".png\" alt=\"" ^ text ^ "\" />", text)
;;
let html_of_snapshot ?(current=false) snapshot =
  let img, text = icon_and_text snapshot.ts_result in
    (if current then "<div class=\"current\">" else "<div>") ^
      img ^ " <div>" ^ text ^ "<br />" ^ (string_of_snapshot snapshot.ts_snapshot) ^ "</div>" ^ "</div>"
;;

let href_snapshot ?(result=false) s =
  if result then
    html_of_snapshot s
  else
    string_of_snapshot s.ts_snapshot;;

let render_snapshot ch ({ ts_snapshot = (year, month, day, build); } as snapshot) past future =
  let snapshot_name = Printf.sprintf "%04i.%02i.%02i-%03i" year month day build in
  let next () = Printf.fprintf ch "<img class=\"next\" src=\"static/images/next.png\" alt=\"next\" />" in
    RenderCommon.html_head ~file:"" ~title:snapshot_name ch;
    Printf.fprintf ch "<div class=\"global builds\">\n";
    List.iter (fun snap -> output_string ch (html_of_snapshot snap); next ()) past;
    output_string ch (html_of_snapshot ~current:true snapshot);
    List.iter (fun snap -> next (); output_string ch (html_of_snapshot snap)) future;
    Printf.fprintf ch "</div><div class=\"clear\"></div>\n";

    Printf.fprintf ch "<ul>\n";
    List.iter
      (fun module_ ->
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
	   in acc [] [] [] [] [] module_.tm_platforms
	 in
	 let fold_platforms lst =
	   List.fold_left
	     (fun str p ->
		str ^ (RenderCommon.render_platform p.tp_platform)
	     ) "" lst
	 in
	 let perhaps title lst =
	   if List.length lst > 0 then
	     Printf.fprintf ch " <span class=\"%s\">%s</span>: %s" (safe_string_of_result (List.hd lst).tp_result) title (fold_platforms lst);
	 in	     
	   Printf.fprintf ch "<li>%s" module_.tm_module;
	   begin match past with
	       [] -> ()
	     | prev :: _ ->
		 try
		   let prev = List.find (fun { tm_module = tm_module' } -> module_.tm_module = tm_module') prev.ts_modules in
		     match prev.tm_revision, module_.tm_revision with
			 Git { (* FIXME, verify vg_repository = prev_repository;*) (*vg_branch = : string; FIXME, verify branches, if they differ it's possibly a contract breach (have to ponder merges though*) vg_commit = prev_commit; }, Git { vg_repository = repo; (*vg_branch : string;*) vg_commit = curr_commit; } ->
			   Printf.fprintf ch " %s" (href_git_commitdiff repo ~current:curr_commit ~previous:prev_commit);
		       | _ -> () (* FIXME *)
		 with
		     Not_found -> () (* FIXME? *)
	   end;
	   perhaps "errors" e;
	   perhaps "deperrors" d;
	   perhaps "warnings" w;
	   perhaps "success" s;
	   perhaps "unknown" u;
	   Printf.fprintf ch "</li>";
      ) snapshot.ts_modules;
    Printf.fprintf ch "</ul>\n";

    RenderCommon.html_foot ch;
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
 		if (not ((* FIXME, depend on time *) Sys.file_exists diff_file_full)) ||
		  (let stat_target, stat_self =
		     Unix.stat diff_file_full, stat_self
		   in stat_target.Unix.st_mtime < stat_self.Unix.st_mtime)
		then
		  begin let ch = open_out (Filename.concat target_dir diff_file) in
		  let p = output_string ch in
		    RenderCommon.html_head ~title:"Diff" ch;
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
		    RenderCommon.html_foot ch;
		    close_out ch;
		  end;
		let ins = List.fold_left (fun acc line -> if (begins_with "+" line) && not (begins_with "+++" line) then acc + 1 else acc) 0 diff in
		let del = List.fold_left (fun acc line -> if (begins_with "-" line) && not (begins_with "---" line) then acc + 1 else acc) 0 diff in
		  "[<a href=\"" ^ diff_file ^ "\">+" ^ (string_of_int ins) ^ "/-" ^ (string_of_int del) ^ "</a>]"
	      end
	    else ""
      | _ -> "n/a" (* FIXME *)
;;

let equal_modules a b =
  a.tm_module = b.tm_module &&
  (same_revision a.tm_module a.tm_revision b.tm_revision) &&
  a.tm_result = b.tm_result (* FIXME *) &&
  List.fold_left2
  (fun acc a' b' ->
     acc &&
       a'.tp_platform = b'.tp_platform &&
      a'.tp_result = b'.tp_result &&
      true (* FIXME tp_builds *)
  ) true a.tm_platforms b.tm_platforms
;;

let render_snapshot_table (columns, hash) (logs : tr_logs) =
  let ch = open_out (Filename.concat target_dir "snapshots.html") in
  let p = output_string ch in
    RenderCommon.html_head ~title:"Snapshots" ch;

    p "<table><tr><th></th>";
    let rec iter lst =
      match lst with
	  [] -> ()
	| snapshot :: [] ->
	    p ("<th>" ^ (href_snapshot ~result:true snapshot) ^ "</th>");
	| snapshot :: tl ->
	    p ("<th>" ^ (href_snapshot ~result:true snapshot) ^ "</th>");
	    p ("<th>" ^ "<img class=\"next\" src=\"static/images/next.png\" alt=\"next\" />" ^ "</th>");
	    iter tl
    in iter (List.rev logs);
      p "<th></th>";
    p "</tr>";
    let hash_lst =
      let hash_lst = ref [] in
	Hashtbl.iter
	  (fun module_name arr ->
	     hash_lst := (module_name, arr) :: !hash_lst
	  ) hash;
	List.sort (fun (a, _) (b, _) -> compare (String.lowercase a) (String.lowercase b)) (!hash_lst)
    in
    List.iter
    (fun (module_name, arr) ->
       p "<tr>";
       p ("<th>" ^ (href_module module_name) ^ "</th>");
       Array.iteri
	 (fun i mopt ->
	    match mopt with
		None ->
		  p "<td class=\"unknown\">no data</td>";
		  (if (i+1) < columns then p "<td class=\"next\"></td>");
	      | Some mcurr ->
		  let equal =
		    (if (i+1) < columns then
		       match arr.(i+1) with
			   None -> false
			 | Some mnext ->
			     if (href_diff ~current:mnext ~previous:mcurr) (* FIXME *) = "" && (equal_modules mcurr mnext) then true else false
		     else false) &&
		      (if i > 0 then
			 match arr.(i-1) with
			   None -> true (* above we want to ensure we render the last one *)
			 | Some mprev ->
			     if (href_diff ~current:mcurr ~previous:mprev) (* FIXME *) = "" && (equal_modules mcurr mprev) then true else false
		       else true)
		  in
		    if equal then
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
			 str ^ " " ^ (RenderCommon.render_platform p.tp_platform)
		      ) "" lst
		  in
		  let perhaps lst =
		    if List.length lst > 0 then
		      let result = (List.hd lst).tp_result in
		      let img, text = icon_and_text ~small:true result in
			Printf.fprintf ch "<li class=\"%s\">%s:%s</li>" (safe_string_of_result result) img (* text takes too much space here *) (fold_platforms lst);
		  in
		    Printf.fprintf ch "<ul>";
		    perhaps e;
		    perhaps d;
		    perhaps w;
		    perhaps s;
		    perhaps u;
		    Printf.fprintf ch "</ul>";
		    p "</td>";
		      end;
		    (if (i+1) < columns then
		       match arr.(i+1) with
			   None -> p "<td class=\"next\"></td>"
			 | Some mnext ->
			     p ("<td class=\"next\">" ^ (href_diff ~current:mnext ~previous:mcurr) ^ "</td>"));
	 ) arr;
       p ("<th>" ^ (href_module module_name) ^ "</th>");
       p "</tr>";
    ) hash_lst;
  p "</table>";

    RenderCommon.html_foot ch;
    close_out ch;
;;

let snapshots (logs : tr_logs) =
  let module_hash =
    (List.length logs, Hashtbl.create 32)
  in
  let rec part past future =
    match past with
	hd :: tl ->
	  (fun ({ ts_snapshot = (year, month, day, build); } as snapshot) ->
	     let snapshot_name = Printf.sprintf "%04i.%02i.%02i-%03i" year month day build in
	     let snapshot_channel = open_out (Filename.concat target_dir ("snapshot-" ^ snapshot_name ^ ".html")) in
	       render_snapshot snapshot_channel snapshot (List.rev tl) future;
	       acc_snapshot module_hash snapshot (List.rev tl) future;
	       close_out snapshot_channel;
	  ) hd;
	  part tl (hd :: future)
      | [] -> ()
  in
    part logs [];
    render_snapshot_table module_hash logs
;;

let frontpage (logs : tr_logs) =
(*  let previous = List.hd (List.tl logs) in*)
  let current = List.hd logs in
  (* Main page *)
  (* dump support *)
  let ch = open_out (Filename.concat target_dir "action.php") in
    Printf.fprintf ch "<?php require_once('static/trurl.php'); trurl_action(); ?>\n";
    close_out ch;
  (* dump glance *)
    let file = "index.php" in
  let ch = open_out (Filename.concat target_dir file) in
  html_head ~dynamic:"<?php require_once('static/trurl.php');  trurl_global_state(); ?>\n" ~file ~title:"Frontpage" ch;
  let fame, other =
    List.partition
      (fun { tm_result = c1 } (*(_, (c1 : result), _, _)*) ->
	match c1 with
(*	| Perfect*)
	| Information
	  -> true
	| Error	| Dependency_error | Warning | Unknown
	    -> false
      ) current.ts_modules
  in

(*    Printf.fprintf ch "<div class=\"global builds\">\n";

    Printf.fprintf ch "%s" (html_of_snapshot previous); (* FIXME *)
    Printf.fprintf ch "<img class=\"next\" src=\"static/images/next.png\" alt=\"next\" />";
    Printf.fprintf ch "%s" (html_of_snapshot ~current:true current);
    
    Printf.fprintf ch "<div class=\"clear\"></div></div>\n";*)

  debug_endline (Printf.sprintf "Fame: %i (%i other)" (List.length fame) (List.length other));
  let confusion, other =
    List.partition
      (fun { tm_result = c1 } (*(_, (c1 : result), _, _)*) ->
	match c1 with
(*	| Perfect*)
	| Information
	  -> assert false
	| Unknown
	  -> true
	| Error	| Dependency_error | Warning
	  -> false
      ) other
  in
  debug_endline (Printf.sprintf "Confusion: %i (%i other)" (List.length confusion) (List.length other));
  

  (* Hall of Fame/Shame/Confusion *)
(*  let fst3 (project, _ (*result*), platforms) =
    "<li>" ^ ((href_of_project project) ^ " " ^ (render_platform_images platforms)) ^ "</li>"
  in*)
  let fst4 { tm_module = project; (*tl_time = special;*) tm_platforms = platforms } =
    "<li>" ^ ((href_of_project project) (*^ (match special with None -> "" | Some _ -> " (building)")*) ^ " " ^ (render_platform_images platforms)) ^ "</li>"
  in
  Printf.fprintf ch "<div class=\"halls\">\n";
  List.iter
    (fun (id, title, list, f) ->
      if List.length list > 0 then begin
	Printf.fprintf ch "<div class=\"%s\">\n<h1>%s</h1>\n" id title;
	Printf.fprintf ch "<ul>\n";
	List.iter
	  (fun item ->
	    Printf.fprintf ch "%s\n" (f item);
	  ) list;
	Printf.fprintf ch "</ul>\n";
	Printf.fprintf ch "</div>\n";
      end;
    ) (
      [
	("hall_of_fame", "Successful<img src=\"static/images/star.png\" alt=\"\" />", ((*if enable_debug_features then ("Fame", Information, []) :: ("Fame", Information, []) :: ("Fame", Information, []) :: ("Fame", Information, []) :: ("Fame", Information, []) :: fame else*) fame), fst4);
(*	("hall_of_shame", "Hall of Shame<img src=\"static/images/bat.png\" alt=\"bat\" />", (((*if enable_debug_features then [("Shame", Error, []); ("Shame", Error, []); ("Shame", Error, []); ("Shame", Error, [])] else*) []) (* @ shame*)), fst4); (* move items here after 3 bats *)*)
	("hall_of_confusion", "Hall of Confusion", confusion, fst4);
      ]);
  Printf.fprintf ch "</div>\n";
  (* Embarassing Mitsakes *)

  RenderEmbarrassment.render ch (*list*) other;

  if List.length other = 0 && List.length confusion = 0 then begin
    Printf.fprintf ch "<div id=\"other\"><h1>Zarro Boogs found.<img src=\"love.png\" /></h1></div>"
  end;

  html_foot ch;

  close_out ch;
;;
