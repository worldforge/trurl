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

let tm_to_string tv = Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;

let tm_to_iso8601 tv = (* This has the nice property of containing no spaces *) Printf.sprintf "%04i-%02i-%02iT%02i:%02i:%02i" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;

let tm_to_iso8601_utc tv = (* This has the nice property of containing no spaces *) Printf.sprintf "%04i-%02i-%02iT%02i:%02i:%02iZ" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;

let tm_to_cvs_iso8601 tv = Printf.sprintf "%04i-%02i-%02i %02i:%02i:%02i UTC" (tv.Unix.tm_year + 1900) (tv.Unix.tm_mon + 1) tv.Unix.tm_mday (tv.Unix.tm_hour) (tv.Unix.tm_min) (tv.Unix.tm_sec)
;;

let iso8601_to_tm_utc str =
  let f =     (fun year month day hour minute second -> { Unix.tm_year = year - 1900; tm_mon = month - 1; tm_mday = day; tm_hour = hour; tm_min = minute; tm_sec = second; (* following is ignored: *) tm_wday = 0; tm_yday = 0; tm_isdst = false; })
  in
  (try
     Scanf.sscanf str "%04i-%02i-%02iT%02i:%02i:%02iZ" f
   with
       Scanf.Scan_failure _ ->
	 Scanf.sscanf str "%04i-%02i-%02iT%02i:%02i:%02i+00:00" f)
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
