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

(*
 * Show loading of the file.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Resource%t" eflush

let debug_resource =
   create_debug (**)
      { debug_name = "resource";
        debug_description = "display resource operations";
        debug_value = false
      }

type ('a, 'b, 'c) rsrc =
   { resource_data : 'c;
     resource_join : ('a, 'b, 'c) rsrc -> ('a, 'b, 'c) rsrc -> ('a, 'b, 'c) rsrc;
     resource_extract : ('a, 'b, 'c) rsrc -> 'b;
     resource_improve : ('a, 'b, 'c) rsrc -> 'a -> ('a, 'b, 'c) rsrc;
     resource_close : ('a, 'b, 'c) rsrc -> string -> ('a, 'b, 'c) rsrc
   }

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
