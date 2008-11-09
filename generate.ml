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

open ExtLib;;

let http_root = "http://yellow.worldforge.org/trurl/client.php";;
let http_source_root = "http://yellow.worldforge.org/trurl/client/source";;

let client dir log_root =
  let ch = open_out (dir ^ "/client.php") in
    Printf.fprintf ch
"<?php
$http_root = '%s';
$http_source_root = '%s';
$root = '%s';
$log_root = '%s';
include('static/client.php');
?>
" http_root http_source_root Build.global_root log_root;
    close_out ch;
;;

(*
let bootstrap template target =
  let template' = input_file template in
  let template'' = Pcre.replace ~pat:"@@HTTP_ROOT@@" ~templ:("'" ^ http_root ^ "'") template' in
    output_file ~filename:target ~text:template''
;;*)

let safe str =
  Pcre.replace ~rex:(Pcre.regexp "[^-a-z_0-9]") ~templ:"_" (String.lowercase str)
;;

let ready snapshot template target build_list =
  let snapshot' = (Build.snapshot_safe_string snapshot) in
  let replace_lst text lst =
    List.fold_left
      (fun text (pat, templ) ->
	 let templ = String.replace_chars (fun c -> match c with '$' -> "$$" | _ -> String.of_char c) templ in
	   Pcre.replace ~pat ~templ text
      ) text lst
  in
  let template = input_file template in
  let template = Pcre.replace ~pat:"@@SNAPSHOT@@" ~templ:("'" ^ snapshot' ^ "'") template in
  let res = Pcre.exec ~rex:(Pcre.regexp ~flags:[`DOTALL] "^(.*)##MODULE-BEGIN##\n(.*)##MODULE-STEP-BEGIN##\n(.*)##MODULE-STEP-END##\n(.*)##MODULE-END##\n(.*)$") template in
  let group = Pcre.get_substring res in
  let bash_pre, bash_post = group 1, group 5 in
  let bash_mpre, bash_mpost = group 2, group 4 in
  let bash_step = group 3 in
    (* FIXME: dump revisions *)
  let output = Buffer.create 1024 in
  let p = Buffer.add_string output in
    p bash_pre;
    List.iter
      (fun { Build.m_module = m_module; m_steps = m_steps; m_revision = m_revision; m_source_dir = m_source_dir; m_patch = m_patch; } ->
	 let safe_m_module = safe m_module in
	 let (safe_m_patch, safe_m_patch_strip) =
	   match m_patch with
	       None
	     | Some (_, []) ->
		 ("", "")
	     | Some (depth, _) ->
		 (safe_m_module ^ "_has_patch", string_of_int depth)
	 in
	 let m_replace_lst = ["@@MODULE@@", m_module; "@@module@@", safe_m_module; "@@patch@@", safe_m_patch; "@@patch-strip@@", safe_m_patch_strip] in
	 let client_source_dir = (Build.global_root ^ "/client_source/" ^ snapshot') in
	   ignore (Sys.command (Printf.sprintf "test -d '%s' || mkdir -p '%s'" client_source_dir client_source_dir));
	   Sys.chdir (m_source_dir);
	   ignore (Sys.command (Printf.sprintf "tar czf \"%s\"/%s.tar.gz --exclude=CVS --exclude=.git  ." client_source_dir safe_m_module));
	   (match m_patch with
		None
	      | Some (_, []) ->
		  ()
	      | Some (_, files) ->
		  ignore (Sys.command (Printf.sprintf "rm -f \"%s\"/%s.patch" client_source_dir safe_m_module));
		  List.iter
		    (fun (a, b) ->
		       ignore (Sys.command (Printf.sprintf "diff -u -N \"%s\" \"%s\" >> \"%s\"/%s.patch" a b client_source_dir safe_m_module));
		    ) files
	   );
	   (*@@tar up correct revision FIXME: use git-tarish to enable going back in time*)
	   p (replace_lst bash_mpre m_replace_lst);
	   let render_step =
	     (fun { Build.s_step = s_step; s_command = s_command; s_environment = s_environment; s_required = s_required; } ->
		let safe_s_step = safe s_step in
		let s_replace_lst = m_replace_lst @ ["@@STEP@@", s_step; "@@step@@", safe_s_step; ("@@COMMAND@@", List.fold_left (fun acc (k, v) -> Printf.sprintf "%s=\"%s\" %s" k v acc) s_command s_environment); ("@@&&@@", if s_required then "&&" else ";")] in
		  (* FIXME, s_required should check if last step and not add &&, also needs scoping {} for it to work properly *)
		  p (replace_lst bash_step s_replace_lst);
	     )
	   in
	   let rec gen_steps steps =
	     match steps with
		 [] -> ()
	       | step :: [] -> (* ignore required *)
		   render_step step
	       | ({ Build.s_required = s_required; } as step) :: tl ->
		   if not s_required then
		     p "{\n";
		   render_step step;
		   gen_steps tl;
		   if not s_required then
		     p "}\n";
	   in gen_steps m_steps;
(*	   List.iter
	     (fun { Build.s_step = s_step; s_command = s_command; s_environment = s_environment; s_required = s_required; } ->
		let safe_s_step = safe s_step in
		let s_replace_lst = m_replace_lst @ ["@@STEP@@", s_step; "@@step@@", safe_s_step; ("@@COMMAND@@", List.fold_left (fun acc (k, v) -> Printf.sprintf "%s=\"%s\" %s" k v acc) s_command s_environment); ("@@&&@@", if s_required then "&&" else "")] in
		  (* FIXME, s_required should check if last step and not add &&, also needs scoping {} for it to work properly *)
		  p (replace_lst bash_step s_replace_lst);
	     ) m_steps;*)
	   p (replace_lst bash_mpost m_replace_lst);
      ) build_list;
    p bash_post;

    output_file ~filename:target ~text:(Buffer.contents output)
;;

let instructions snapshot dir build_list =
  let dir = dir ^ "/instructions" in
    ignore (Sys.command (Printf.sprintf "test -d '%s' || mkdir -p '%s'" dir dir));
(*    bootstrap (Build.global_root ^ "/bootstrap.template.sh") (dir_posix ^ "/bootstrap.sh");*)
    ready snapshot (Build.global_root ^ "/ready.posix.sh") (dir ^ "/ready.posix.sh") build_list;
;;

let main () =
(*if Array.length (Sys.argv) = 2 && Sys.argv.(1) = "--generate" then begin*)
  
  client (Build.global_root ^ "/../public_html") (* FIXME, read from config or similar *) Build.global_logs;
  let build_list = (Build.get_build_list ()) in
  let (snapshot_dir, snapshot) = Build.make_snapshot_dir () in
    Build.store_revisions snapshot_dir build_list;
    instructions snapshot snapshot_dir build_list;
(*end else begin

end*)
;;

main ();;

