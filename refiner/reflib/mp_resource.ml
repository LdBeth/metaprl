(*
 * Resource management.
 * Each resource provides four operations:
 *    1. Create a new, empty resource
 *    2. Join two resource providers
 *    3. Extract a value from the resource
 *    4. Add a value to the resource
 *
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Mp_debug
open Printf

open Refiner.Refiner.TermType
open Refiner.Refiner.TermMeta
open Refiner.Refiner.RefineError

(*
 * Show loading of the file.
 *)
let _ =
   show_loading "Loading Mp_resource%t"

let debug_resource =
   create_debug (**)
      { debug_name = "resource";
        debug_description = "display resource operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Data is linked so that parts can be retrieved by name.
 *)
type 'data data =
   DataBase of 'data
 | DataLabel of string * 'data data
 | DataLink of 'data * 'data data

(*
 * Resources are saved when they are labeled.
 *)
type ('info, 'result, 'data, 'arg) t =
   { resource_info : ('info, 'result, 'data, 'arg) info;
     resource_data : 'data data;
     resource_list : (string * ('info, 'result, 'data, 'arg) t) list ref
   }

(*
 * these are the methods for modifying a resource.
 *)
and ('info, 'result, 'data, 'arg) info =
   { resource_join : 'data -> 'data -> 'data;
     resource_extract : 'data -> 'result;
     resource_improve : 'data -> 'info -> 'data;
     resource_improve_arg :
        'data ->
        string ->               (* Name of the new resource *)
        string array ->         (* Names of the context vars *)
        string array ->         (* Names of the new variables *)
        term list ->            (* Arguments *)
        term list ->            (* Parameters *)
        meta_term ->            (* Rule statement *)
        'arg ->                 (* Extra arguments *)
        'data;
     resource_close : 'data -> string -> 'data
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Get the data from a link.
 *)
let rec get_data = function
   DataBase data ->
      data
 | DataLabel (_, data) ->
      get_data data
 | DataLink (data, _) ->
      data

let find_data name data =
   let rec search = function
      DataBase _ ->
         eprintf "Warning: resources for %s are not found%t" name eflush;
         get_data data
    | DataLabel (name', data) ->
         if name' = name then
            get_data data
         else
            search data
    | DataLink (_, next) ->
         search next
   in
      search data

(*
 * Create an initial resource.
 *)
let create info data =
   { resource_info = info;
     resource_data = DataBase data;
     resource_list = ref []
   }

(*
 * Merge two resources.
 *)
let join { resource_info = info; resource_data = data1; resource_list = resources } { resource_data = data2 } =
   { resource_info = info;
     resource_data = DataBase (info.resource_join (get_data data1) (get_data data2));
     resource_list = resources
   }

(*
 * Extract a value from the data.
 *)
let extract { resource_info = info; resource_data = data } name =
   info.resource_extract (find_data name data)

let extract_top { resource_info = info; resource_data = data } =
   info.resource_extract (get_data data)

(*
 * Add some new info to the resource.
 *)
let improve { resource_info = info; resource_data = data; resource_list = resources } info' =
   { resource_info = info;
     resource_data = DataLink (info.resource_improve (get_data data) info', data);
     resource_list = resources
   }

let improve_list { resource_info = info; resource_data = data; resource_list = resources } info_list =
   { resource_info = info;
     resource_data = DataLink (List.fold_left info.resource_improve (get_data data) info_list, data);
     resource_list = resources
   }

let improve_arg { resource_info = info; resource_data = data; resource_list = resources } name cvars vars args params mterm arg =
   { resource_info = info;
     resource_data = DataLink (info.resource_improve_arg (get_data data) name cvars vars args params mterm arg, data);
     resource_list = resources
   }

let improve_arg_fail name _ _ _ _ _ _ _ _ =
   raise (RefineError (name, StringError "resource method 'improve_arg' is not implemented"))

let rec improve_list { resource_info = info; resource_data = data; resource_list = resources } infos =
   let improve' = info.resource_improve in
   let rec fold data = function
      h :: t ->
         fold (DataLink (improve' (get_data data) h, data)) t
    | [] ->
         data
   in
      { resource_info = info;
        resource_data = fold data infos;
        resource_list = resources
      }

let wrap { resource_info = info; resource_data = data; resource_list = resources } f =
   { resource_info = info;
     resource_data = DataLink (f (get_data data), data);
     resource_list = resources
   }

(*
 * Label a resource.
 *)
let label info name =
   let { resource_info = info'; resource_data = data; resource_list = resources } = info in
      { resource_info = info';
        resource_data = DataLabel (name, data);
        resource_list = resources
      }

let close info name =
   let { resource_info = info'; resource_data = data; resource_list = resources } = info in
   let info'' =
      { resource_info = info';
        resource_data = DataLabel (name, DataLink (info'.resource_close (get_data data) name, data));
        resource_list = resources
      }
   in
      resources := (String.capitalize name, info'') :: !resources;
      info''

(*
 * Get an resource value from the list of labeled resources.
 *)
let find { resource_list = resources } name =
   List.assoc (String.capitalize name) !resources

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
