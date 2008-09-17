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

(** A module for command-line option parsing *)

(** A type for parsing callbacks. Callback is a function to bind information
    obtained during parsing into some data structure *)
type 'a callback =  
  | Bool      of ('a -> 'a)           (** Called when an option without      
                                          parameter is parsed *)
  | Parameter of (string -> 'a -> 'a) (** Called when non-optional parameter  
                                          is parsed *)
  | Optional  of (string -> 'a -> 'a) (** Called when optional parameter is  
                                          parsed; if the parameter is omitted
                                          then empty string is passed as an
                                          argument *)

(** Result of parsing *)
type 'a result = 
  | Success    of 'a * string list    (** Success. The first param is the      
                                          result value, the second -         
                                          unbound parameters *)
  | Incomplete of 'a * string list * string list
                                      (** The first param is the result value,   
                                          the second - unbound parameters, 
                                          the third - warning messages on     
                                          missing parameters *)


(** List of option short names (without leading '-'), long names (without leading '--') and callback functions *)
type 'a pattern = (string * string * 'a callback) list

(** Function to read from configuration file to list of options. Parameter   
    is configuration file name. Configuration file consists of list of       
    options in the format [-option[=parameter]], one option per line. '#' is   
    recognized as a comment being encountered at the first position of the   
    line *)
val read  : string -> string list

(** Options parser. Takes command line, pattern and initial value        
    and returns result value  *)
val parse : string list -> 'a pattern * 'a -> 'a result









