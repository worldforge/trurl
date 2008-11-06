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

let render_embarassment ({ tm_module = project; tm_result = result; tm_revision = tm_revision; tm_platforms = platforms }
                        : tr_module) history =
  let platforms_perfect, platforms_other =
      List.partition
	(fun { tp_result = result }(*(platform, platform_result, logs)*) ->
	  result (*platform_result*) = (* Perfect *) Information
	) platforms
    in
    let platforms_confusion, platforms_mistakes =
      List.partition
	(fun { tp_result = result } ->
           result = Unknown
	) platforms_other
    in
    let platforms_mistakes =
      AlertSort.sort_alert_high_to_low (fun { tp_platform = platform; tp_result = result } -> platform, result) platforms_mistakes in
    "<li id=\"" ^ (xml_escape_attribute project) ^ "\" class=\"" ^ (safe_string_of_result result) ^ "\">" ^
      (href_of_project project) ^

      (Printf.sprintf "<table>%s</table>" (module_history_to_table_row ~merge:true ~always_arrows:true ~show_snapshot_id:true ~limit:3 history)) ^

      (if List.length platforms_mistakes > 0 (* Only needed to validate during testing *) then
	 begin
	   render_mistakes platforms_mistakes
	end else " (visible since in debug mode)") ^

      "</li>"
;;

let render ch (other : tr_module list) history_hash =
  (
    let (id, title, list, f) = (
      
	("other", "Build problems",
	other, render_embarassment);
      )
    in
    (* sort list by result status *)
    let list = AlertSort.sort_alert_high_to_low (fun { tm_module = a; tm_result = b } (*(a, b, _, c)*) -> a, b) list in
    match list with
      [] -> ()
    | { tm_result = first_item_result } :: _ -> begin
	(* extract first items result status *)
	let previous_item_result = ref first_item_result in
	  Printf.fprintf ch "<div class=\"%s\">\n" (*"<h1>%s</h1>\n"*) id (*title*);
	  List.iter
	    (fun ({ tm_result = this_item_result; tm_module = id; tm_platforms = platforms }) ->
	       Printf.fprintf ch "<a class=\"%s\" href=\"#%s\">%s</a>%s\n" (safe_string_of_result this_item_result) (xml_escape_attribute id) id (render_platform_gears platforms);
	    ) list;
	  Printf.fprintf ch "<ul class=\"%s\">\n" (safe_string_of_result first_item_result);
	  List.iter
	    (fun ({ tm_result = this_item_result; tm_module = tm_module; } as item) ->
	       if this_item_result <> !previous_item_result then begin
		 previous_item_result := this_item_result;
		 Printf.fprintf ch "</ul>\n";
		 Printf.fprintf ch "<ul class=\"%s\">\n" (safe_string_of_result this_item_result);
	       end;
	       Printf.fprintf ch "%s\n" (f item (Hashtbl.find history_hash tm_module));
	    ) list;
	  Printf.fprintf ch "</ul>\n";
	  Printf.fprintf ch "</div>\n";
	end
    );
;;

