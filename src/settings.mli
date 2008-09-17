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

(** Settings manupulation: a functions to maintain a (mutable) structure with 
   various parameters indexed by string keys *)

(** A mutable settings holder *)
type 'a t = 
  { 
    get      : string -> 'a option;  (** gets parameter value *)
    set      : string -> 'a -> unit; (** sets value *)
    help     : unit   -> string;     (** gets help/info etc. *)
    toString : unit   -> string;     (** gets string representaion *)
  } 

(** Result of command-line parsing *)
type 'a result =
  | Ok of 'a t * string list (** parsing ok, params are parsed and bound
                                settings and the list of unbound parameters *)

  | Warnings of 'a t * string list * string list             
                                   (** there are some warnings produced.       
                                       First two params are those as before,   
                                       the last one is the list of warning         
                                       messages *)
 
(** Tag of settings values: integers, boolean flags, strings *)
type tag = Int of int | Flag | Str of string 

(** Kind of option values: integer, boolean, string or accumulated string (with
    possibly empty delimiter) *)
type kind = Number | Switch | String | Accu of string

(** Optionality of option parameter *)
type opt = Optional | Mandatory

(** {2 Settings Initializers } *)

type init = string list -> tag result
(** Settings initializer is the function that can be applied to a list of options
    to provide the settings holder. Initializers are implemented in such a way that 
    they may be extended (customized) by adding some additional keys/options *)

(** Default settings constructor. Does nothing: [default settings] returns 
    initializer that returns [Ok (settings, args)] being applied to [args] *)
val default : tag t -> init 

(** A constructor of an empty initializer. [empty ()] returns initializer that
    returns [Ok (empty, args)] where [empty] - empty settings, [args] - all arguments
    passed to initializer *)
val empty : unit -> init (*string list -> tag result*)

(**                                                                            
    [customize init options binder] customizes the initializer [init] by adding the 
     ability of parsing [options]. [binder] is a function to bind some 
     default values to mandatory options. Example:
      
    {v customize (empty ())
       ["o", String, Mandatory, "\t - specify output file name"]
       (fun () -> ["o", Str "a.out"])
    v}

    returns option parser that recognizes option "-o" with string argument.
    Quadruple ["o", Str "", true, "\t - specify output file name"] defines
    option name, parameter type, optionality and help info. Tabbing symbol within 
    help string serves as field delimiter; getting help on options will 
    align the text to field bounds. Normally the repeating usage of the option 
    overrides the previous one, so [-o a -o b] results in [b]; alternatively 
    [Accu ";"] tag forces option parameters to accumulate so [-o a -o b] results 
    in ["a;b"] (like [-I] option for [cc]).
*)
val customize : 
    init  -> (string * string * kind * opt * string) list -> (unit -> (string * tag) list) -> init



