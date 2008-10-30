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

let tm_to_string tv = Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour + 1) (tv.Unix.tm_min + 1) (tv.Unix.tm_sec + 1)
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
      html_head ~ch ~file:(project_url project) ~title:project;
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

let html_of_snapshot ?(current=false) snapshot =
  let img, text =
    match snapshot.ts_result with
        Error -> "error", "Error"
      | Dependency_error -> "error", "Error (dependency)"
      | Warning -> "warning", "Warning"
      | Information -> "ok", "Success"
      | Unknown -> "error", "Internal error"
  in
    (if current then "<div class=\"current\">" else "<div>") ^
      "<img src=\"static/images/" ^ img ^ ".png\"> <div>" ^ text ^ "<br />" ^ (string_of_snapshot snapshot.ts_snapshot) ^ "</div>" ^
      "</div>"
;;

let render_snapshot ch ({ ts_snapshot = (year, month, day, build); } as snapshot) past future =
  let snapshot_name = Printf.sprintf "%04i.%02i.%02i-%03i" year month day build in
  let next () = Printf.fprintf ch "<img class=\"next\" src=\"static/images/next.png\" />" in
    RenderCommon.html_head ~ch ~file:"" ~title:snapshot_name;
    Printf.fprintf ch "<div class=\"global builds\">\n";
    List.iter (fun snap -> output_string ch (html_of_snapshot snap); next ()) past;
    output_string ch (html_of_snapshot ~current:true snapshot);
    List.iter (fun snap -> next (); output_string ch (html_of_snapshot snap)) future;
    Printf.fprintf ch "</div><div class=\"clear\"></div>\n";
    RenderCommon.html_foot ch;
;;

let snapshots (logs : tr_logs) =
  let rec part past future =
    match past with
	hd :: tl ->
	  (fun ({ ts_snapshot = (year, month, day, build); } as snapshot) ->
	     let snapshot_name = Printf.sprintf "%04i.%02i.%02i-%03i" year month day build in
	     let snapshot_channel = open_out (Filename.concat target_dir ("snapshot-" ^ snapshot_name ^ ".html")) in
	       render_snapshot snapshot_channel snapshot (List.rev tl) future;
	       close_out snapshot_channel;
	  ) hd;
	  part tl (hd :: future)
      | [] -> ()
  in
    part logs []
;;

let frontpage (logs : tr_logs) =
  let previous = List.hd (List.tl logs) in
  let current = List.hd logs in
  (* Main page *)
  (* dump support *)
  let ch = open_out (Filename.concat target_dir "action.php") in
    Printf.fprintf ch "<?php require_once('static/trurl.php'); trurl_action(); ?>\n";
    close_out ch;
  (* dump glance *)
  let ch = open_out (Filename.concat target_dir "index.php") in
  html_head ~ch ~file:"index.html" ~title:"Frontpage";
    Printf.fprintf ch "<?php require_once('static/trurl.php');  trurl_global_state(); ?>\n";
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

    Printf.fprintf ch "<div class=\"global builds\">\n";

    Printf.fprintf ch "%s" (html_of_snapshot previous); (* FIXME *)
    Printf.fprintf ch "<img class=\"next\" src=\"static/images/next.png\">";
    Printf.fprintf ch "%s" (html_of_snapshot ~current:true current);
    
    Printf.fprintf ch "<div class=\"clear\"></div></div>\n";

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
	("hall_of_fame", "Successful<img src=\"static/images/star.png\" alt=\"star\" />", ((*if enable_debug_features then ("Fame", Information, []) :: ("Fame", Information, []) :: ("Fame", Information, []) :: ("Fame", Information, []) :: ("Fame", Information, []) :: fame else*) fame), fst4);
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
