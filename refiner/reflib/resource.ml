(*
 * Resource management.
 * Each resource provides four operations:
 *    1. Create a new, empty resource
 *    2. Join two resource providers
 *    3. Extract a value from the resource
 *    4. Add a value to the resource
 *
 *)

open Debug
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
     resource_improve : ('a, 'b, 'c) rsrc -> 'a -> ('a, 'b, 'c) rsrc
   }

(*
 * $Log$
 * Revision 1.3  1998/06/12 13:55:30  jyh
 * Modified resources.
 *
 * Revision 1.1  1998/05/28 22:09:57  jyh
 * Updated Makefiles.
 *
 * Revision 1.1  1998/05/28 15:01:07  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1997/04/28 15:51:34  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.1  1996/09/25 22:52:02  jyh
 * Initial "tactical" commit.
 *
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
