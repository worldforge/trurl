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

open Object;;

let atlas_debug =
  try Sys.getenv "ATLAS_DEBUG" <> "" with Not_found -> false

type state = M | L | S | I | F | N | P | E;;

(* XXX clean me up!!! *)
let pop_hd lst = match lst with head :: tail -> tail | [] -> assert false;;
class packed =
object (s)
  val mutable state = [E]
  val mutable current : atlasobject list = []
  val mutable buffer = ""
  val mutable plusbuffer = ""
  val mutable namebuffer = []
  val mutable objects = []
  method encode obj =
    let escape s =
      let str = ref "" in
      for i=0 to (String.length s)-1 do
        str := !str ^ 
          match String.get s i with
            '+' -> "+2b"
          | '[' -> "+5b"
          | ']' -> "+5d"
          | '(' -> "+28"
          | ')' -> "+29"
          | '@' -> "+40"
          | '#' -> "+23"
          | '$' -> "+24"
          | '=' -> "+3d"
          | '\n' -> "+0a"
          | '\r' -> "+0d"
          | x -> (String.make 1 x)
      done;
      !str
    in
    let name = ref "" in
    let rec enc obj =
      match obj with
	Map m ->
          let x = "[" ^ !name in
          let y = (String.concat "" (List.map (fun (k, d) -> name := ((escape k) ^ "="); enc d) m)) ^ "]" in
          name := "";
	  x ^ y
      | List l ->
          let x = !name in
          name := "";
	  "(" ^ x ^ (String.concat "" (List.map enc l)) ^ ")"
      | String s ->
	  "$" ^ !name ^ (escape s)
      | Int i ->
	  "@" ^ !name ^ (string_of_int i)
      | Float f ->
	  "#" ^ !name ^ (string_of_float f) in
    let output = enc obj in
    if atlas_debug then Printf.eprintf "// ATLAS(OUT): '%s'\n" output;
    output
  method private add_to_current obj =
    match List.hd current with
      Map m ->
        (match current with
          [] -> current <- [Map ((List.hd namebuffer, obj) :: m)]
        | head :: tail -> current <- Map ((List.hd namebuffer, obj) :: m) :: tail);
        namebuffer <- pop_hd namebuffer
    | List l ->
        (match current with
          [] -> current <- [List (l @ [obj])]
        | head :: tail -> current <- List (l @ [obj]) :: tail)
    | Float _ | Int _ | String _ -> assert false
  method private finish_current =
    (match List.hd state with
      E|M|L -> ()
    | S ->
	s#add_to_current (String buffer);
	state <- pop_hd state
    | I ->
	s#add_to_current (Int (int_of_string buffer));
	state <- pop_hd state
    | F ->
	s#add_to_current (Float (float_of_string buffer));
	state <- pop_hd state
    | P | N -> assert false
    );
    buffer <- "";
  method decode data =
    if atlas_debug then Printf.eprintf "// ATLAS(IN): '%s'\n" data;
    objects <- [];
    for i=0 to (String.length data)-1 do
      let data = String.get data i in
      match data with
      | '+' when (List.hd state = N || List.hd state = S) ->
	  (match List.hd state with
	    N -> state <- P :: state
	  | S -> state <- P :: state
	  | (E|P|F|I|L|M) -> assert false
	  )
      | '[' ->
	  s#finish_current;
	  (match List.hd state with
            E -> state <- M :: state
	  | L -> state <- M :: state
	  | M -> state <- N :: M :: state
	  | (P|N|F|I|S) -> assert false
	  );
	  current <- Map [] :: current
      | ']' ->
	  s#finish_current;
	  (match List.hd state with
	    M ->
	      state <- pop_hd state;
	      if List.length state == 1 then objects <- List.hd current :: objects
		(*| E -> objects <- List.hd current :: objects*)
	  | (E|P|N|F|I|S|L) -> assert false
	  );
	  let tmp = List.hd current in
	  current <- pop_hd current;
	  if List.length state != 1 then
	    s#add_to_current tmp;
      | '(' ->
	  s#finish_current;
	  (match List.hd state with
	    E -> state <- L :: state
	  | L -> state <- L :: state
	  | M -> state <- N :: L :: state
	  | (P|N|F|I|S) -> assert false
	  );
	  current <- List [] :: current
      | ')' ->
	  s#finish_current;
	  (match List.hd state with
	    L ->
	      state <- pop_hd state;
	      if List.length state == 1 then objects <- List.hd current :: objects
		(*| E -> objects <- List.hd current :: objects*)
	  | (E|P|N|F|I|S|M) -> assert false
	  );
	  let tmp = List.hd current in
	  current <- pop_hd current;
	  if List.length state != 1 then
	    s#add_to_current tmp;
      | '@' ->
	  s#finish_current;
	  (match List.hd state with
            L -> state <- I :: state
	  | M -> state <- N :: I :: state
	  | (E|P|N|F|I|S) -> assert false
	  )
      | '#' ->
	  s#finish_current;
	  (match List.hd state with
            L -> state <- F :: state
	  | M -> state <- N :: F :: state
	  | (E|P|N|F|I|S) -> assert false
	  )
      | '$' ->
	  s#finish_current;
	  (match List.hd state with
            L -> state <- S :: state
	  | M -> state <- N :: S :: state
	  | (E|P|N|F|I|S) -> assert false
	  )
      | '=' ->
	  namebuffer <- buffer :: namebuffer;
	  buffer <- "";
	  (match List.hd state with
            N ->
	      state <- pop_hd state
	  | (E|P|F|I|S|L|M) -> assert false
	  )
      | '\n' ->
	  print_endline "Bad character in input: newline, ignoring.";
      | '\r' ->
	  print_endline "Bad character in input: carriage return, ignoring.";
      | '-' | '+' when (List.hd state = I) && (String.length buffer = 0) ->
	  buffer <- buffer ^ (String.make 1 data)
      | '0'..'9' when (List.hd state = I || List.hd state = F) ->
	  buffer <- buffer ^ (String.make 1 data)
      | '.' | 'e' | 'E' | '-' | '+' | 'n' | 'a' when List.hd state = F -> (* also handles nan *)
	  buffer <- buffer ^ (String.make 1 data)
      | '0'..'9'|'A'..'F'|'a'..'f' when List.hd state = P ->
	  plusbuffer <- plusbuffer ^ (String.make 1 data);
	  if String.length plusbuffer == 2 then
	    begin
	      buffer <- buffer ^ (String.make 1 (char_of_int (int_of_string ("0x" ^ plusbuffer))));
	      plusbuffer <- "";
	      state <- pop_hd state
	    end
      | _ ->
	  (match List.hd state with
	    S -> buffer <- buffer ^ (String.make 1 data)
	  | N -> buffer <- buffer ^ (String.make 1 data)
	  | (E|P|F|I|L|M) as state ->
	      (Printf.eprintf
		 "State %c when (S|N) expected, data = '%c'.\n"
		 (match state with
		   S -> 'S'
		 | N -> 'N'
		 | E -> 'E'
		 | P -> 'P'
		 | F -> 'F'
		 | I -> 'I'
		 | L -> 'L'
		 | M -> 'M')
		 data
	      );
	      assert false
	  )
    done;
    objects
end

let rec pprint_atlasobject obj =
  match obj with
    Map m ->
      print_endline "map[";
      List.iter (fun (k, d) -> print_string (k ^ "="); pprint_atlasobject d;) m;
      print_endline "]map";
  | List l ->
      print_endline "list[";
      List.iter pprint_atlasobject l;
      print_endline "]list";
  | String s -> print_endline ("string[" ^ s ^ "]");
  | Int i -> print_endline ("int[" ^ (string_of_int i) ^ "]");
  | Float f -> print_endline ("float[" ^ (string_of_float f) ^ "]");;


let test () =
  print_endline "decoding...";
  let codec = new packed in
  List.iter pprint_atlasobject (codec#decode "[$foo=poof][(bar=)][#baz=4.5][@quux=11][@id=17$name=Fred +28the +2b great+29#weight=1.5(args=@1@2@3)]");
  List.iter print_endline (List.map codec#encode (codec#decode "[$foo=poof][(bar=)][#baz=4.5][@quux=11][@id=17$name=Fred +28the +2b great+29#weight=1.5(args=@1@2@3)]"));
