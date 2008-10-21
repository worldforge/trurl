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

let render_embarassment ({ tm_module = project; tm_result = result; tm_revision = tm_revision; tm_platforms = platforms }
                        : tr_module) =
  let platforms_perfect, platforms_other =
      List.partition
	(fun { tp_result = result }(*(platform, platform_result, logs)*) ->
	  result (*platform_result*) = (* Perfect *) Information
	) platforms
    in
    let platforms_confusion, platforms_mistakes =
      List.partition
	(fun { tp_result = result } (*(platform, platform_result, logs)*) ->
           result = Unknown
	) platforms_other
    in
    let platforms_mistakes =
      AlertSort.sort_alert_high_to_low (fun { tp_platform = platform; tp_result = result } -> platform, result) platforms_mistakes in
    "<li id=\"" ^ (xml_escape_attribute project) ^ "\" class=\"" ^ (safe_string_of_result result) ^ "\">" ^
      (href_of_project project) (*^ (match special with None -> "" | Some _ -> " (building)")*) ^
      (if List.length platforms_perfect > 0 then begin " (success on " ^ (render_platform_images platforms_perfect) ^ ")" (* TODO: use an UL when I can be bothered *) end else "") ^
      (if List.length platforms_confusion > 0 then begin " (confused on " ^ (render_platform_images platforms_confusion) ^ ")" (* TODO: use an UL when I can be bothered; not sure it's wise, it's not as much a list from a structure perspective *) end else "") ^

      (if List.length platforms_mistakes > 0 (* Only needed to validate during testing *) then
	begin
	  "<ul>\n"
	  ^ (List.fold_left (fun str { tp_platform = platform; tp_result = platform_result; (*tp_logs = platform_logs; tp_time = time;*) tp_builds = tp_builds } ->
                               let platform_logs = (List.hd tp_builds).tb_logs in
	    str (* folded string *)
	    ^ "<li class=\"" ^ (safe_string_of_result platform_result) ^ "\">\n"
	    ^ "<img src=\"static/platforms/" ^ platform ^ platform_image_version_mistake ^ ".png\" alt=\"" ^ platform ^ "\" />" (*FIXME^ (time_as_gears_or_time time)*) ^ "\n" ^
	      begin (* TODO: How do we give extensible knowledge of build-step sorting? *)
(*	(List.map
	  (fun (step, (result, log_filename, (*float,*) lines)) ->
	  ) data;*)
		let { f_step = step; f_result = step_result; f_filename = log_filename; f_lines = lines } =
		  List.find (fun { f_result = step_result; } -> platform_result = step_result) (sort_steps platform_logs)
		in
		let error_messages = (List.filter (fun (_, _, line_result) -> line_result = platform_result) lines) in
                  (mklink ("generated/" ^ log_filename) step) ^ ":" ^
		if List.length error_messages > 0 then
		  "<br />\n"
		  ^ List.fold_left
		    (fun str (line_no, line_data, line_result) -> str ^
		      (Printf.sprintf (*"<dl>\n<dt>%i</dt><dd><tt>%s</tt></dd>\n</dl>\n"*) "<tt>%i: %s</tt><br/>\n" line_no (escape_html line_data))
		    ) "" (up_to 10 error_messages)
                    ^ (if List.length error_messages > 10 then
                         Printf.sprintf "<tt>Log contains %i more messages.</tt><br/>\n" (List.length error_messages - 10)
                       else "")
		else (* Meta told us something bad happened. *)
		  " Non-zero exit status.<br />Check meta for specifics."
	      end
	    ^ "</li>\n") "" platforms_mistakes)
	  ^ "</ul>\n"
	end else " (visible since in debug mode)") ^

      "</li>"
(*    let fst3 (x, y, z) = (x ^ " " ^ (List.fold_left (fun str (platform, result) -> str ^ "<img src=\"platforms/" ^ platform ^ ".png\" />") "" z)) in*)
;;

let render ch (other : tr_module list) =
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
	  Printf.fprintf ch "<div class=\"%s\">\n<h1>%s</h1>\n" id title;
	  List.iter
	    (fun ({ tm_result = this_item_result; tm_module = id; tm_platforms = platforms }) ->
	       Printf.fprintf ch "<a class=\"%s\" href=\"#%s\">%s</a>%s\n" (safe_string_of_result this_item_result) (xml_escape_attribute id) id (render_platform_gears platforms);
	    ) list;
	  Printf.fprintf ch "<ul class=\"%s\">\n" (safe_string_of_result first_item_result);
	  List.iter
	    (fun ({ tm_result = this_item_result } as item) ->
	       if this_item_result <> !previous_item_result then begin
		 previous_item_result := this_item_result;
		 Printf.fprintf ch "</ul>\n";
		 Printf.fprintf ch "<ul class=\"%s\">\n" (safe_string_of_result this_item_result);
	       end;
	       Printf.fprintf ch "%s\n" (f item);
	    ) list;
	  Printf.fprintf ch "</ul>\n";
	  Printf.fprintf ch "</div>\n";
	end
    );
;;

