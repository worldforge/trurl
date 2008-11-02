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

let tm_to_string tv = Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;

let tm_to_iso8601 tv = (* This has the nice property of containing no spaces *) Printf.sprintf "%04i-%02i-%02iT%02i:%02i:%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;

let tm_to_iso8601_utc tv = (* This has the nice property of containing no spaces *) Printf.sprintf "%04i-%02i-%02iT%02i:%02i:%02iZ" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;

let tm_to_cvs_iso8601 tv = (* This has the nice property of containing no spaces *) Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i UTC" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;

let md5 str =
  Cryptokit.transform_string (Cryptokit.Hexa.encode ()) (Cryptokit.hash_string (Cryptokit.Hash.md5 ()) str)
;;

let ends_with pattern string =
  if String.length pattern > String.length string then
    false
  else
    String.sub string (String.length string - String.length pattern) (String.length pattern) = pattern
;;

let begins_with pattern string =
  if String.length pattern > String.length string then
    false
  else
    String.sub string 0 (String.length pattern) = pattern
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
    (" <a href=\"" ^ html_root ^ "\">Frontpage</a> <a href=\"" ^ html_root ^ "snapshots.html\">Snapshot Overview</a>")
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

let fmt' uptime =
  let rec fmt acc =
    match acc with
	[] -> 
	  Printf.sprintf "%.1f seconds" uptime
      | ((name, seconds) :: acc) ->
	  let t = uptime /. (float seconds) in
	    if t >= 1. then begin
	      if (truncate (t *. 10.)) = 10 then
	        Printf.sprintf "%.0f %s" t name
	      else
	        Printf.sprintf "%.1f %ss" t name
	    end else
	      fmt acc
  in
    fmt ["year", 365*24*60*60; "month", 30*24*60*60; "week", 7*24*60*60; "day", 24*60*60; "hour", 60*60; "minute", 60; "second", 1]
;;

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

let render_platform platform =
  let (platform, version) = split platform '-' in
    "<img src=\"static/images/platforms/" ^ platform ^ platform_image_version ^ ".png\" alt=\"" ^ platform ^ "\" /> " ^ version
;;

let render_platform_images platforms =
  (List.fold_left
     (fun str { tp_platform = platform; tp_result = result; tp_builds = builds; (*tp_time = time;*) (*tp_logs = logs*) } ->
	str ^ (render_platform platform)
     ) "" platforms)
;;

let render_platform_gears platforms =
  (List.fold_left (fun str { tp_platform = platform; tp_result = result; (*tp_time = time; tp_logs = logs*) } -> str (*^ (time_as_gears time)*)) "" platforms)
;;
