(*
 * Resource management.
 * Each resource provides four operations:
 *    1. Create a new, empty resource
 *    2. Join two resource providers
 *    3. Extract a value from the resource
 *    4. Add a value to the resource
 *
 *)

type ('a, 'b, 'c) rsrc =
   { resource_data : 'c;
     resource_join : ('a, 'b, 'c) rsrc -> ('a, 'b, 'c) rsrc -> ('a, 'b, 'c) rsrc;
     resource_extract : ('a, 'b, 'c) rsrc -> 'b;
     resource_improve : ('a, 'b, 'c) rsrc -> 'a -> ('a, 'b, 'c) rsrc
   }

(*
 * $Log$
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
