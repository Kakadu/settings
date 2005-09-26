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
 *  (enclosed in the file LGPL).
 **************************************************************************)

module StringMap = Map.Make (struct type t = string let compare = compare end)

type 'a holder = 'a StringMap.t ref
type 'a t =
{
 get      : string -> 'a option;
 set      : string -> 'a -> unit; 
 help     : unit -> string;
 toString : unit -> string;
} 

type 'a result = Ok of 'a t * string list | Warnings of 'a t * string list * string list
type    tag    = Int of int | Flag | Str of string 
type    kind   = Number | Switch | String | Accu of string
type    opt    = Optional | Mandatory

let create ts =
  let holder = ref (StringMap.empty) in
  {
   get      = (fun key -> try Some (StringMap.find key !holder) with Not_found -> None | x -> raise x);
   set      = (fun key valu -> holder := StringMap.add key valu !holder; ());
   help     = (fun () ->
       try 
	 let Str help = StringMap.find "$$memo" !holder in
	 let helps    = Str.split (Str.regexp "\n") help in
	 let fields   = List.map (Str.split (Str.regexp "\t")) helps in
	 let fldnum   = List.fold_left (fun m l -> max (List.length l) m) 0 fields in
	 let fldwidth = Array.make fldnum 0 in
	 let iterator = Array.of_list fields  in
	 Array.iteri (fun i _ -> 
	   fldwidth.(i) <- fst (Array.fold_left 
	     (fun (m, i) e -> 
	       match e with
	       | []      -> (m, i+1)
	       | h :: tl -> iterator.(i) <- tl; (max (String.length h) m), i+1
	     ) (0, 0) iterator)
         ) fldwidth;
	 List.fold_left 
	   (fun curr fields ->
	     (snd (List.fold_left (fun (i, curr) str -> (i+1), (curr ^ str ^ (String.make (fldwidth.(i) - (String.length str)) ' '))) (0, curr) fields)) ^ "\n"
	   ) "" fields
       with
       | Not_found -> ""
       | x -> raise x
     );
   toString = (fun () ->
     StringMap.fold 
       (fun key valu str -> 
	 (if str = "" then "" else str ^ "\n") ^ (Printf.sprintf "%s=%s" key (ts valu))
       ) 
       !holder 
       ""
     )
  }  

type init = string list -> tag result

let tagtoString = function Int n -> string_of_int n | Str s -> s | Flag -> ""

let default x = fun args -> Ok (x, args)

let customize conf options init =
  (fun args ->
    let res = conf args in
    let conf, frees, warns =
      match res with
      |	Ok (c, f) -> c, f, []
      |	Warnings (c, f, w) -> c, f, w
    in
    List.iter (fun (key, valu) -> conf.set key valu) (init ()); 
    let memo = 
      List.fold_left
	(fun s (key, _, _, m) -> 
	  (if s = "" then "" else s ^ "\n") ^ "-" ^ key ^ m
	) 
	"" 
	options 
    in
    begin match conf.get "$$memo" with
    | None -> conf.set "$$memo" (Str memo)
    | Some (Str str) -> conf.set "$$memo" (Str (str ^ "\n\n" ^ memo))
    end;
    let filter = 
      List.map 
	(fun (key, tag, opt, _) -> 
	  (key, 
	   match tag with
	   | Accu delim ->
	       if opt = Optional
	       then 
		 Options.Optional  
		   (fun s conf -> 
		     match conf.get key with
		     | Some (Str x) ->
			 if x = "" && s <> ""
			 then conf.set key (Str (delim ^ s))
	                 else conf.set key (Str (x ^ delim ^ s));
			 conf

		     | None -> 
			 conf.set key (Str s); conf
		   )
	       else Options.Parameter (fun s conf -> conf.set key (match conf.get key with  None -> Str (delim ^ s) | Some (Str x) -> Str (x ^ delim ^ s)); conf)
	       
           | Number -> 
	       if opt = Optional
	       then 
		 Options.Optional  
		   (fun s conf -> 
		     match conf.get key with
		     | Some (Int x) ->
			 if x = -1 && s <> ""
			 then conf.set key (Int (int_of_string s)); 
			 conf

		     | None -> 
			 conf.set key (Int (if s = "" then -1 else (int_of_string s))); 
			 conf
		   )
	       else Options.Parameter (fun s conf -> conf.set key (Int (int_of_string s)); conf)
		 
	   | String -> 	       
	       if opt = Optional
	       then Options.Optional 
		   (fun s conf -> 
		     match conf.get key with
		     | Some (Str x) -> 
			 if x = "" && s <> "" then  conf.set key (Str s); 
			 conf
			   
                     | None -> conf.set key (Str s); conf
		   )
		   
	       else Options.Parameter (fun s conf -> conf.set key (Str s); conf) 
		 
	   | Switch -> Options.Bool (fun conf -> conf.set key Flag; conf)
	  )
	) 
	options 
    in
    let res = Options.parse frees (filter, conf) in
    let conf, frees, warns =
      match res with
      | Options.Success    (conf, frees) -> conf, frees, warns
      | Options.Incomplete (conf, frees, incompl) ->
	  conf, frees, warns @ (List.map (fun s -> Printf.sprintf "Command-line option %s parameter missing" s) incompl)
    in
    match warns with
    | [] -> Ok       (conf, frees)
    | _  -> Warnings (conf, frees, warns)
  )

let empty () = default (create tagtoString)

