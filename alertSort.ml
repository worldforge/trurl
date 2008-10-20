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

let highest_alert =
  (fun (a : result) (b : result) ->
    match a, b with
(*type result = Perfect | Error | Dependency_error | Warning | Information | Unknown*)
      Error, _
    | _, Error -> Error
    | Dependency_error, _
    | _, Dependency_error -> Dependency_error
    | Warning, _
    | _, Warning -> Warning
(*    | Perfect, _
    | _, Perfect -> Perfect*)
    | Information, _
    | _, Information -> Information
    | Unknown, Unknown -> Unknown
  )

let highest_alert_compare =
  (fun (a : result) (b : result) ->
    if a = b then 0 else
      match a, b with
	Error, _ -> 1
      | _, Error -> -1
      | Dependency_error, _ -> 1
      | _, Dependency_error -> -1
      | Warning, _ -> 1
      | _, Warning -> -1
(*      | Perfect, _ -> 1
      | _, Perfect -> -1*)
      | Information, _ -> 1
      | _, Information -> -1
      | Unknown, Unknown -> 0 (* not reached *)
  )

  let sort_alert_high_to_low ext =
    List.sort
      (fun item1 item2 ->
	let (project1, result1), (project2, result2) = ext item1, ext item2 in
	let res1 = highest_alert_compare result1 result2 in
	let res1 = if res1 > 0 then -1 else if res1 < 0 then 1 else res1 in
	let res2 = if res1 = 0 then compare project1 project2 else res1 in
	res2
      )
  ;;

