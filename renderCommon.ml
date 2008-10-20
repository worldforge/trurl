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

let html_head ~ch ~file ~title =
  Printf.fprintf ch "%s\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n%s<title>%s - WorldForge's Autobuilder</title>\n<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\" />\n<link rel=\"stylesheet\" href=\"/trurl/static/trurl_shared.css\" type=\"text/css\" />\n<link rel=\"stylesheet\" href=\"/trurl/static/trurl_frontpage.css\" type=\"text/css\" />\n</head>\n<body>\n<div class=\"top_menu\">%s<!--<ul>%s</ul>-->%s</div>\n"
    doctype_xhtml11
    meta_refresh title
    (if enable_debug_features then "Debug mode activated. This page will refresh every " ^ (if meta_refresh_interval = 10 then "ten" else Printf.sprintf "%i" meta_refresh_interval) ^ " seconds." else "")
    (List.fold_left
       (fun str (link, ext, title) ->
          str ^ "<li><a href=\"" ^ link ^ "\">" ^ title ^ (if ext then " <img src=\"/trurl/static/external.png\" alt=\"external link\" />" else "") ^ "</a></li>"
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
    "<a href=\"http://www.worldforge.org/\">WorldForge</a>'s <a href=\"/trurl/\">Autobuilder</a>";
;;

let html_foot ?(valid=true) ch =
  (if valid then
     Printf.fprintf ch "<p>\n<a href=\"http://validator.w3.org/check?uri=referer\">\n<img style=\"border:0;width:88px;height:31px\" src=\"static/valid-xhtml11\" alt=\"Valid XHTML 1.1\" />\n</a>\n<a href=\"http://jigsaw.w3.org/css-validator/\">\n<img style=\"border:0;width:88px;height:31px\" src=\"static/vcss\" alt=\"Valid CSS!\" />\n</a>\n</p>\n");
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
  (if time = None then "<img src=\"static/gears.png\" alt=\"building\" />" else "")

let time_as_gears_or_time (time : float option) =
  match time with
      Some seconds -> "(" ^ (fmt' seconds) ^ ")"
    | None -> time_as_gears time

let render_platform_images platforms =
  (List.fold_left (fun str { tp_platform = platform; tp_result = result; tp_builds = builds; (*tp_time = time;*) (*tp_logs = logs*) } -> str ^ "<img src=\"static/platforms/" ^ platform ^ platform_image_version ^ ".png\" alt=\"" ^ platform ^ "\" />" (*^ (time_as_gears ((List.hd builds).tb_time))*)) "" platforms)
;;

let render_platform_gears platforms =
  (List.fold_left (fun str { tp_platform = platform; tp_result = result; (*tp_time = time; tp_logs = logs*) } -> str (*^ (time_as_gears time)*)) "" platforms)
;;
