(**************************************************************************
 *  Copyright (C) 2005
 *  Dmitri Boulytchev (db@tepkom.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA    
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

type 'a callback = Bool of ('a -> 'a) | Parameter of (string -> 'a -> 'a) | Optional of (string -> 'a -> 'a)
type 'a result   = Success of 'a * string list | Incomplete of 'a * string list * string list    
type 'a pattern  = (string * string * 'a callback) list

let read fname =  
  let ch = open_in fname in
  let rec inner l =    
    try
      let arg = input_line ch in
      if (String.length arg > 0) && (arg.[0] <> '#') then 
	try 
          let i = String.index arg '=' in
          inner 
            (
             (String.sub arg (i+1) ((String.length arg)-i-1)) :: 
             (String.sub arg 0 i) :: 
             l
            )
	with
	| Not_found -> inner (arg :: l)
	| x -> raise x
      else inner l
    with
    | End_of_file -> List.rev l
    | x -> raise x    
  in
  let x = inner [] in 
  close_in ch;
  x

let parse args (patterns, conf) =
  let find patterns option =
    let key, longkey, c =  (List.find (fun (key, longkey, c) -> "-" ^ key = option || "--" ^ longkey = option) patterns) in 
    if option = "-" ^ key then key, c else longkey, c
  in
  let callback, conf, frees, incompl, last =
    List.fold_left
      (fun (callback, conf, frees, incompl, last) arg ->
        if arg.[0] = '-' 
        then
          try
            let conf, incompl = match callback with
	    | None -> conf, incompl
	    | Some (Optional f) -> (f "" conf), incompl
	    | _    -> conf, (if last = "" then incompl else last :: incompl)
	    in
	    let arg, cb = find patterns arg in 
	    match cb with
	    | Bool f -> None, f conf, frees, incompl, ""
	    | Parameter _ -> (Some cb), conf, frees, incompl, arg
	    | Optional  _ -> (Some cb), conf, frees, incompl, ""
          with
          | Not_found -> None, conf, (arg :: frees), incompl, ""
          | x -> raise x             
        else
          match callback with
          | None -> None, conf, (arg :: frees), incompl, ""
          | Some (Parameter f) | Some (Optional f) -> None, (f arg conf), frees, incompl, ""
      )
      (None, conf, [], [], "")
      args
  in
  let conf =
    match callback with
    | Some (Optional f) -> f "" conf
    | _ -> conf
  in  
  let incompl = if last = "" then incompl else last :: incompl in
  match incompl with
  | [] -> Success    (conf, List.rev frees)
  | _  -> Incomplete (conf, List.rev frees, List.rev incompl)
