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
open RenderCommon;;
open ExtLib;;
open Util;;

let dump_module { tm_module = project; tm_result = tm_result; tm_platforms = platforms; tm_revision = tm_revision; } history ch =
  let pr title description = Printf.fprintf ch "<dt>%s</dt>\n<dd>%s</dd>\n" title description in
  let dl result = Printf.fprintf ch "<dl class=\"%s\">" (safe_string_of_result result) in
  let dl' () = Printf.fprintf ch "</dl>" in
    Printf.fprintf ch "<div class=\"%s\">" (safe_string_of_result tm_result);
    Printf.fprintf ch "<h1>%s</h1>" project;
    trace ~skip:true ("dump_module." ^ project ^ ".module_history_to_table_row") (fun () -> Printf.fprintf ch "<table>%s</table>" (module_history_to_table_row ~limit:5 ~merge:true ~always_arrows:true ~show_snapshot_id:true history)) ();
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
  let module_hash = calc_module_hash logs.tl_snapshots (* FIXME? *) in
    Hashtbl.iter
(*    List.iter*)
      (fun tm_module arr (*({ tm_module = tm_module; tm_result = tm_result; tm_platforms = tm_platforms; tm_revision = tm_revision; } as m)*) ->
	 let m = (* find last built module *)
	   let m = ref None in
	     Array.iter (fun m' -> if m' <> None then m := m') arr;
	     Option.get (!m)
	 in
	 let ch = open_out (Filename.concat target_dir (project_url tm_module)) in
	   html_head ~file:(project_url tm_module) ~title:tm_module ch;
	   Printf.fprintf ch "<div class=\"other\">";
	   trace ~skip:false ("render.project_pages.dump_module." ^ m.tm_module) (fun () -> dump_module m (arr) ch) ();
	   Printf.fprintf ch "</div>";
	   html_foot ch;
	   close_out ch;
      ) module_hash
      (*(List.hd logs).ts_modules*)
;;

let href_snapshot ?(result=false) s =
  if result then
    html_of_snapshot s
  else
    string_of_snapshot s.ts_snapshot;;

let render_snapshot ch ({ ts_snapshot = (year, month, day, build); } as snapshot) past future =
  let snapshot_name = Printf.sprintf "%04i.%02i.%02i-%03i" year month day build in
  let next rtl = Printf.fprintf ch "%s" ("<img class=\"next\" src=\"static/images/next" ^ (if rtl then "-rtl" else "-ltr") ^ ".png\" alt=\"next\" />") in
  let rtl = true in
    RenderCommon.html_head ~file:"" ~title:snapshot_name ch;
    Printf.fprintf ch "<div class=\"global builds\">\n";
    List.iter (fun snap -> output_string ch (html_of_snapshot snap); next rtl) past;
    output_string ch (html_of_snapshot ~current:true snapshot);
    List.iter (fun snap -> next rtl; output_string ch (html_of_snapshot snap)) future;
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
		str ^ (RenderCommon.render_platform p)
	     ) "" lst
	 in
	 let perhaps title lst =
	   if List.length lst > 0 then
	     Printf.fprintf ch " <span class=\"%s\">%s</span>: %s" (safe_string_of_result (List.hd lst).tp_result) title (fold_platforms lst);
	 in	     
	   Printf.fprintf ch "<li>%s" module_.tm_module;
(*	   begin match past with
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
	   end;*)
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

let render_snapshot_table (columns, hash) (logs : tr_logs) =
  let file = "snapshots.html" in
  let ch = open_out (Filename.concat target_dir file) in
  let p = output_string ch in
  let columns = min 5 columns in
  let rtl = true in
    RenderCommon.html_head ~title:"Snapshots" ~file ch;

    p "<table><tr><th></th>";
    let rec iter lst =
      match lst with
	  [] -> ()
	| snapshot :: [] ->
	    p ("<th>" ^ (href_snapshot ~result:true snapshot) ^ "</th>");
	| snapshot :: tl ->
	    p ("<th>" ^ (href_snapshot ~result:true snapshot) ^ "</th>");
	    p ("<th>" ^ "<img class=\"next\" src=\"static/images/next" ^ (if rtl then "-rtl" else "-ltr") ^ ".png\" alt=\"next\" />" ^ "</th>");
	    iter tl
    in iter (List.rev (List.drop (List.length (logs.tl_snapshots (* FIXME: add tip *)) - columns) (List.rev (logs.tl_snapshots))));
      p "<th></th>";
    p "</tr>";

    let hash_lst =
      let hash_lst = ref [] in
	Hashtbl.iter
	  (fun module_name arr ->
	     hash_lst := (module_name, arr) :: !hash_lst
	  ) hash;
	List.sort ~cmp:(fun (a, _) (b, _) -> compare (String.lowercase a) (String.lowercase b)) (!hash_lst)
    in
      List.iter
	(fun (module_name, arr) ->
	   p (module_history_to_table_row ~title:module_name ~limit:columns arr ~rtl)
	) hash_lst;

  p "</table>";

    RenderCommon.html_foot ch;
    close_out ch;
;;

let render_timeline (columns, hash) (logs : tr_logs) =
  let file = "timeline.html" in
  let ch = open_out (Filename.concat target_dir file) in
  let p = output_string ch in
    RenderCommon.html_head ~file ~title:"Snapshots" ch;

    p "<table class=\"timeline\">";

    let hash_lst =
      let hash_lst = ref [] in
	Hashtbl.iter
	  (fun module_name arr ->
	     hash_lst := (module_name, arr) :: !hash_lst
	  ) hash;
	List.sort ~cmp:(fun (a, _) (b, _) -> compare (String.lowercase a) (String.lowercase b)) (!hash_lst)
    in
      List.iter
	(fun (module_name, arr) -> (* FIXME: precalc width, and do something more interesting! (interdependencies would be nice) *)
	   p (module_history_to_table_row ~merge:true ~title:module_name ~limit:5 (*columns*) ~padding:`left ~always_arrows:true ~show_snapshot_id:true arr)
	) hash_lst;

  p "</table>";

    RenderCommon.html_foot ch;
    close_out ch;
;;

let snapshots (logs : tr_logs) =
  let module_hash =
    (List.length logs.tl_snapshots, Hashtbl.create 32)
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
    part logs.tl_snapshots [];
    render_snapshot_table module_hash logs;
    render_timeline module_hash logs;
;;

let frontpage (logs : tr_logs) module_hash current_modules =
  (* Main page *)
  (* dump support *)
  let ch = open_out (Filename.concat target_dir "action.php") in
    Printf.fprintf ch "<?php require_once('static/trurl.php'); trurl_action(); ?>\n";
    close_out ch;
  (* dump glance *)
    let file = "index.php" in
  let ch = open_out (Filename.concat target_dir file) in
  let hosts_seen =
    let hosts_acc = Hashtbl.create 16 in
      List.iter
	(fun m ->
	   List.iter
	     (fun p ->
		List.iter
		  (fun b ->
		     Hashtbl.replace hosts_acc b.tb_host ()
		  ) p.tp_builds
	     ) m.tm_platforms
	) logs.tl_tip;
      Hashtbl.length hosts_acc
  in
  html_head ~dynamic:(Printf.sprintf "<?php $trurl_hosts_seen = %i; require_once('static/trurl.php');  trurl_global_state(); ?>\n" hosts_seen) ~file ~title:"Frontpage" ch;
  let fame, other =
    List.partition
      (fun { tm_result = c1 } (*(_, (c1 : result), _, _)*) ->
	match c1 with
(*	| Perfect*)
	| Information
	  -> true
	| Error	| Dependency_error | Warning | Unknown
	    -> false
      ) current_modules
  in

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
  
  RenderEmbarrassment.render ch ~tip:logs.tl_tip other module_hash;

  if List.length other = 0 && List.length confusion = 0 then begin
    Printf.fprintf ch "<div id=\"other\"><h1>Zarro Boogs found.<img src=\"love.png\" /></h1></div>"
  end;

  html_foot ch;

  close_out ch;
;;
