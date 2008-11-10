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
open ExtLib;;

(*type data = {
  r_result : result;
}
type t = (string, data) Hashtbl.t*)

let load () =
  try
    let data = input_file ~bin:true "trurl.states" in
    let codec = new Codec_packed.packed in
    let objects = codec#decode data in
      match objects with
          [] -> Object.Map []
        | hd :: _ -> hd
  with Sys_error _ -> Object.Map []
;;

let save new_results =
  let codec = new Codec_packed.packed in
  let data = codec#encode new_results in
    output_file ~filename:"trurl.states" ~text:data
;;

let filter_map f l =
  List.map (fun x -> match x with None -> failwith "impossible (filter_map)" | Some x' -> x') (List.filter ((<>) None) (List.map f l))
;;

let merge (old_results : Object.atlasobject) latest_modules =
  let new_results =
    Object.Map
      (List.map
            (fun { tm_module = tm_module; tm_platforms = tm_platforms } ->
               (tm_module,
                (Object.Map 
                   (filter_map
                      (fun { tp_platform = tp_platform; tp_result = tp_result; (*tp_time = tp_time;*) tp_builds = tp_builds; } ->
                         if (List.fold_left (fun acc build -> acc +. build.tb_time) 0.0 tp_builds) > 0.0 then
                           Some (tp_platform, Object.Map [("result", Object.String (safe_string_of_result tp_result))])
                         else
                           None
                      ) tm_platforms)))
            ) latest_modules)
  in
  let changeset =
    let extract results =
      List.flatten
        (List.map
        (fun (key, platforms) ->
           (match platforms with
                Object.Map lst ->
                  List.map
                    (fun (platform, data) ->
                       (key, platform),
                       (match data with
                            Object.Map lst ->
                              (try
                                 match List.assoc "result" lst with
                                     Object.String str -> Some str
                                   | _ -> None
                               with Not_found -> None)
                          | _ -> None)
                    ) lst
              | _ -> []
           )
        )
      (match results with
          Object.Map lst -> lst
        | _ -> failwith "results not map, FIXME"))
    in
    let old =
      extract old_results
    in
    let new_ =
      extract new_results
    in
    let rec mkchangeset changeset old new_ =
      match new_ with
          [] -> changeset
        | (key, None) :: tl ->
            mkchangeset changeset old tl
        | (key, Some res) :: tl ->
            let old' = try List.assoc key old with Not_found -> None in
              match old' with
                  None ->
                    mkchangeset ((key, (old', res)) :: changeset) old tl
                | Some old'' ->
                    if old'' <> res then
                      mkchangeset ((key, (old', res)) :: changeset) old tl
                    else
                      mkchangeset changeset old tl
    in
      mkchangeset [] old new_
  in
  let new_results =
    (* insert missing old results into new results *)
    match old_results, new_results with
        Object.Map oldlst, Object.Map newlst ->
          begin
            let rec merge2_if_missing newlst oldlst =
              match oldlst with
                  [] -> Object.Map newlst
                | (key, odata) :: tl ->
                    let new' = try Some (List.assoc key newlst) with Not_found -> None in
                      match new' with
                          Some ndata ->
                            merge2_if_missing ((key, ndata) :: (List.remove_assoc key newlst)) tl
                        | None ->
                            merge2_if_missing ((key, odata) :: newlst) tl
            in
            let rec merge1_if_missing newlst oldlst =
              match oldlst with
                  [] -> Object.Map newlst
                | (key, odata) :: tl ->
                    let new' = try Some (List.assoc key newlst) with Not_found -> None in
                      match new' with
                          Some ndata ->
                            (match odata, ndata with
                                 Object.Map odata, Object.Map ndata ->
                                   merge1_if_missing ((key, merge2_if_missing ndata odata) :: (List.remove_assoc key newlst)) tl
                               | _ -> failwith "platform not map, FIXME")
                        | None ->
                            merge1_if_missing ((key, odata) :: newlst) tl
            in
              merge1_if_missing newlst oldlst
          end
      | _ -> failwith "results not map, FIXME"
  in
  (new_results, changeset)

(*let changeset_save changeset (* returns false if empty changeset *) =*)

let now () =
  let tm = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) (tm.Unix.tm_mday) (tm.Unix.tm_hour) (tm.Unix.tm_min) (tm.Unix.tm_sec)

let hooks changeset latest_modules =
  (* when updating topic first check if the generated one is identical to the existing one *)
  (* We *need* a way to skip in-progress builds. FIXME *)
  let log = open_out_gen [Open_wronly;Open_append;Open_creat] 0o644 "trurl.states.log" in
    List.iter
      (fun ((module_, platform), (past, present)) ->
         Printf.fprintf log "%s  %s(%s): %s -> %s\n" (now ()) module_ platform (match past with None -> "<none>" | Some res -> res) present
      ) changeset;
    List.iter
      (fun ((module_, platform), (past, present)) ->
         match Unix.fork () with
             0 ->
               Unix.execv "./hook_changeset" [| "./hook_changeset"; Printf.sprintf "%s: %s on %s (from %s)" module_ present platform (match past with None -> "<none>" | Some res -> res); "http://yellow.worldforge.org/trurl/" |]
           | _ -> ()
      ) changeset;

    begin
      let rec acc errors deperrors warnings logs =
        match logs with
            [] -> errors, deperrors, warnings
          | { tm_result = Error; tm_module = tl_module } :: tl ->
              acc (tl_module :: errors) deperrors warnings tl
          | { tm_result = Dependency_error; tm_module = tl_module } :: tl ->
              acc errors (tl_module :: deperrors) warnings tl
          | { tm_result = Warning; tm_module = tl_module } :: tl ->
              acc errors deperrors (tl_module :: warnings) tl
          | _ :: tl -> acc errors deperrors warnings tl
    in
    let errors, deperrors, warnings =
      acc [] [] [] latest_modules (* XXX probably wrong as we actually want the full list of latest results *)
    in
    let errors, deperrors, warnings =
      List.sort errors, List.sort deperrors, List.sort warnings
    in
    let errors, deperrors, warnings =
      List.fold_left
        (fun a b ->
           a ^ " " ^ b
        ) "" errors,
      List.fold_left
        (fun a b ->
           a ^ " " ^ b
        ) "" deperrors,
      List.fold_left
        (fun a b ->
           a ^ " " ^ b
        ) "" warnings
    in
    let topic =
      let rec topicjoin acc lst =
        match lst with
            [] -> acc
          | (_, "") :: tl -> topicjoin acc tl
          | (a, b) :: tl ->
              topicjoin
                ((if acc = "" then "" else acc ^ " ") ^
                   a ^ "<" ^ b ^ " >") tl
      in
        topicjoin "" ["error", errors; "deperror", deperrors; "warning", warnings]
    in
    let topic =
      if topic = "" then
        "Zarro Boogs!"
      else topic
    in
      match Unix.fork () with
          0 ->
            Unix.execv "./hook_irc" [| "./hook_irc"; topic |]
        | _ -> ()      
    end
;;
