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

open Config
open Types

type callbacks = {
  time : float -> unit;
}
    
let render_line ?(first="") ?(anchor="") ?(link="") ?(pre=false) ch color string =
  ch :=
    (Printf.sprintf
       "%s%s%s<font class=\"%s\">%s%s%s</font>%s%s<br>\n"
       first
       (if anchor <> "" then Printf.sprintf "<a name=\"%s\">" anchor else "")
       (if link <> "" then Printf.sprintf "<a href=\"%s\">" link else "")
       (safe_string_of_result color)
       (if pre then "<tt>" else "")
       (Str.global_substitute (Str.regexp "[<> ]")
	  (fun x -> let x = Str.matched_string x in match x with "<" -> "&lt;" | ">" -> "&gt;" | " " -> "&nbsp;" | x -> x)
	  string
       )
       (if pre then "</tt>" else "")
       (if anchor <> "" then "</a>" else "")
       (if link <> "" then "</a>" else "")
    ) :: !ch
let do_scan line format func =
  try
    (Scanf.sscanf line format func); true
  with
      Scanf.Scan_failure _
    | Failure _
    | End_of_file ->
        false
          
let render_out ~errors ~matched_lines ~total_lines ~warnings ~dep_err =
  if !matched_lines <> !total_lines || !errors > 0 || !warnings > 0 || !dep_err > 0 then
    Printf.sprintf "E%i, W%i%s%s" !errors !warnings (if !dep_err > 0 then Printf.sprintf ", D%i" !dep_err else "") (if !matched_lines <> !total_lines then Printf.sprintf ", ?%i" (!total_lines - !matched_lines) else "")
  else "All ok."
    
let __parse lst ~(cb : callbacks) line =
  let n_tested = ref 0 in
  let (rex, res) =
    (List.find
         (fun (rex, _) ->
	    incr n_tested;
            try
              ignore (Pcre.exec ~rex:(Pcre.regexp ~flags:[`CASELESS(*;`UTF8*)] rex) line); true
            with Not_found ->
              false
              | Pcre.BadPattern _ as e ->
                  prerr_string "Bad pattern: ";
                  error_endline rex;
                  raise e
         ) (lst @ ["(?<!-W)error", Error; "warn", Warning;]))
  in res, (!n_tested, rex)
