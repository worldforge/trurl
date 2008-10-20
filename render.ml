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

let dump_module { tl_module = project; tl_result = tl_result; tl_revisions (*tl_builds = tl_builds*) = tl_revisions; tl_repository = tl_repository; tl_branch = tl_branch; } ch =
     let platforms = (List.hd tl_revisions).tr_platforms in
      let pr title description = Printf.fprintf ch "<dt>%s</dt>\n<dd>%s</dd>\n" title description in
      Printf.fprintf ch "<div class=\"%s\">" (safe_string_of_result tl_result);
      Printf.fprintf ch "<h1>%s</h1>" project;
      Printf.fprintf ch "<dl>";
      Printf.fprintf ch "<dt>%s</dt>" "Repository";
      Printf.fprintf ch "<dd>%s</dd>" (match tl_repository with CVS (root, path) -> "CVS: " ^ root ^ " using path " ^ path | Subversion (root, path) -> "SVN: " ^ root ^ " using path " ^ path | Git origin -> "Git: " ^ origin);
      Printf.fprintf ch "<dt>%s</dt>" "Branch";
      Printf.fprintf ch "<dd>%s</dd>" tl_branch;
      pr "Result" (safe_string_of_result tl_result);
      Printf.fprintf ch "<dt>%s</dt>" "Revisions";
      List.iter
        (fun { tr_revision = tr_revision; tr_platforms = tr_platforms; tr_result = tr_result; } ->
           Printf.fprintf ch "<dd>";
           Printf.fprintf ch "<dl>";
           pr "Revision" (match tr_revision with CVS' time -> "CVS at time " ^ (tm_to_string time) | Subversion' rev -> "SVN: r" ^ (string_of_int rev) | Git' commit -> "Git: " ^ commit);
           pr "Result" (safe_string_of_result tr_result);
           Printf.fprintf ch "<dt>%s</dt>" "Platforms";
           List.iter
             (fun { tp_platform = tp_platform; tp_result = tp_result; tp_builds = tp_builds; } ->
                Printf.fprintf ch "<dd>";
                Printf.fprintf ch "<dl>";
                pr "Platform" tp_platform;
                pr "Result" (safe_string_of_result tp_result);
                Printf.fprintf ch "<dt>%s</dt>" "Builds";
                List.iter
                  (fun { tb_host = tb_host; tb_time = tb_time; tb_start = tb_start; tb_result = tb_result; } ->
                Printf.fprintf ch "<dd>";
                Printf.fprintf ch "<dl>";
                     pr "Host" tb_host;
                     pr "Start" (tm_to_string tb_start);
                     pr "Time" (RenderCommon.fmt' tb_time);
                     pr "Result" (safe_string_of_result tb_result);
                Printf.fprintf ch "</dl>";
                Printf.fprintf ch "</dd>";           
                  ) tp_builds;
                Printf.fprintf ch "</dl>";
                Printf.fprintf ch "</dd>";           
             ) tr_platforms;
           Printf.fprintf ch "</dl>";
           Printf.fprintf ch "</dd>";
        ) tl_revisions;
      Printf.fprintf ch "</dl>";
      Printf.fprintf ch "Build %s.%s" (safe_string_of_result tl_result) (if tl_result = Information then "<img src=\"static/star.png\" alt=\"star\" />" else "");

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
    (fun ({ tl_module = project; tl_result = tl_result; tl_revisions (*tl_builds = tl_builds*) = tl_revisions; tl_repository = tl_repository; tl_branch = tl_branch; } as m) ->
      let ch = open_out (Filename.concat target_dir (project_url project)) in
      html_head ~ch ~file:(project_url project) ~title:project;
      Printf.fprintf ch "<div class=\"other\">";
      dump_module m ch;
      Printf.fprintf ch "</div>";
      html_foot ch;
      close_out ch;
    ) logs
;;

let render_future (logs : tr_logs) =
  let ch = open_out (Filename.concat target_dir ("future.html")) in
    html_head ~ch ~file:("future.html") ~title:"Future";
    Printf.fprintf ch "<div class=\"other\">";
    Printf.fprintf ch "<h1>%s</h1>" "In Progress";
    List.iter
      (fun m ->
         dump_module m ch;
      ) logs;
  Printf.fprintf ch "</div>";
  html_foot ch;
  close_out ch;
;;

(*
let in_progressP (logs : tr_logs) =
  Sys.file_exists "force/build" ||
  List.fold_left
    (fun in_progress module_ ->
       List.fold_left
         (fun in_progress platform ->
            in_progress || platform.tp_time = None
         ) in_progress (List.hd module_.tl_builds).tb_platforms
    ) false logs
;;
*)

let frontpage (transformed_logs : tr_logs(*
(string * LogsTypes.result * LogsTypes.special option *
   (string * LogsTypes.result * LogsTypes.logs) list)
  list*)
) (previous : tr_logs) (future : tr_logs option) =
  (* Main page *)
  (* dump support *)
  let ch = open_out (Filename.concat target_dir "action.php") in
    Printf.fprintf ch "<?php require_once('static/trurl.php'); trurl_action(); ?>\n";
    close_out ch;
  (* dump glance *)
  let ch = open_out (Filename.concat target_dir "index.php") in
  html_head ~ch ~file:"index.html" ~title:"Frontpage";
    Printf.fprintf ch "<?php require_once('static/trurl.php');  trurl_global_state(%s); ?>\n" (if future = None then "false" else "true");
(*    begin
      let render_time { Unix.tm_year = year; tm_mon = month; tm_mday = day; tm_hour = hour; tm_min = minute } =
	Printf.sprintf "%04i-%02i-%02i %02i:%02i UTC" (1900 + year) month day hour minute
      in
      let render_js_time_ago time =
	Printf.sprintf
	  "<script type=\"text/javascript\">
<!--
now = new Date( );
last = new Date( %f * 1000 );

diff = (now.getTime( ) + (now.getTimezoneOffset( ) * 60 * 1000)) - last.getTime( );
diff_days = Math.floor( diff / 1000 / 60 / 60 / 24 );
diff_hours = Math.floor((diff / 1000 / 60 / 60) - (diff_days * 24));
diff_minutes = Math.floor((diff / 1000 / 60) - (diff_hours * 60) - (diff_days * 24 * 60));

document.write('; ');
if (diff_days > 0) {
  document.write(diff_days + 'd ');
}
document.write(diff_hours + 'h ');
document.write(diff_minutes + 'm ago.');
-->
</script>" time
      in
      let render_stat_mtime file =
	let { Unix.st_mtime = mtime } = Unix.stat file; in
	let mtime = Unix.gmtime (mtime) in
	let utc_mtime = fst (Unix.mktime mtime (*-. 3600.0 (* TODO: daylight dst. *)*)) in
	  (render_time mtime ^ render_js_time_ago utc_mtime)
      in
      Printf.fprintf ch "<dl class=\"time\">\n";
        Printf.fprintf ch "<dt>CVS last checked</dt>\n";
        Printf.fprintf ch "<dd>%s</dd>\n" (render_stat_mtime "cvs.forge.update");
      Printf.fprintf ch "<dt>git last checked</dt>\n";
      Printf.fprintf ch "<dd>%s</dd>\n" (render_stat_mtime "git.ember.update");
      Printf.fprintf ch "<dt>Build last started</dt>\n";
      Printf.fprintf ch "<dd>%s</dd>\n" (render_stat_mtime "timestamp_latest_build");
      Printf.fprintf ch "</dl>\n";
    end;*)

(*  let transformed_logs =
    List.map (* PONDER XXX: For now Information == Perfect; UPDATE: now information *is* perfect *)
      (fun (a, (c1 : result), b) ->
	a, (match c1 with
	  Information -> Perfect
	| x -> x), b
      ) transformed_logs
  in*)
  let fame, other =
    List.partition
      (fun { tl_result = c1 } (*(_, (c1 : result), _, _)*) ->
	match c1 with
(*	| Perfect*)
	| Information
	  -> true
	| Error	| Dependency_error | Warning | Unknown
	    -> false
      ) transformed_logs
  in
  let build_to_result logs =
    let img, text =
      match List.fold_left AlertSort.highest_alert Unknown (List.map (fun { tl_result = x } -> x ) logs) with
        Error -> "error", "Error"
      | Dependency_error -> "error", "Error (dependency)"
      | Warning -> "warning", "Warning"
      | Information -> "ok", "Success"
      | Unknown -> "error", "Internal error"
    in "<img src=\"static/" ^ img ^ ".png\"> " ^ text
  in

    Printf.fprintf ch "<p class=\"global builds\">\n";

    Printf.fprintf ch "<span class=\"previous\">%s</span>" (build_to_result previous);
    Printf.fprintf ch "<img class=\"next\" src=\"static/next.png\">";
    Printf.fprintf ch "<span class=\"current\">%s</span>" (build_to_result transformed_logs);
    
    (match future with
         None -> (render_future [])
       | Some future ->
           Printf.fprintf ch "<img class=\"next\" src=\"static/next.png\">";
           Printf.fprintf ch "<span class=\"future\">%s</span>" "<img src=\"static/in_progress.png\" /> <a href=\"future.html\">In progress</a>"; (* FIXME: link to a page *)
           render_future future);

    Printf.fprintf ch "</p>\n";

  debug_endline (Printf.sprintf "Fame: %i (%i other)" (List.length fame) (List.length other));
  let confusion, other =
    List.partition
      (fun { tl_result = c1 } (*(_, (c1 : result), _, _)*) ->
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
  let fst4 { tl_module = project; (*tl_time = special;*) tl_revisions = tl_revisions }
 (*(project, _ (*result*), special, platforms)*) =
     let platforms = (List.hd tl_revisions).tr_platforms in
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
	("hall_of_fame", "Successful<img src=\"static/star.png\" alt=\"star\" />", ((*if enable_debug_features then ("Fame", Information, []) :: ("Fame", Information, []) :: ("Fame", Information, []) :: ("Fame", Information, []) :: ("Fame", Information, []) :: fame else*) fame), fst4);
	("hall_of_shame", "Hall of Shame<img src=\"static/bat.png\" alt=\"bat\" />", (((*if enable_debug_features then [("Shame", Error, []); ("Shame", Error, []); ("Shame", Error, []); ("Shame", Error, [])] else*) []) (* @ shame*)), fst4); (* move items here after 3 bats *)
	("hall_of_confusion", "Hall of Confusion<img src=\"static/confused.png\" alt=\"confused\" />", (((*if enable_debug_features then [("Confusion", Unknown, []); ("Confusion", Unknown, []); ("Confusion", Unknown, []); ("Confusion", Unknown, [])] else*) []) @ confusion), fst4);
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
