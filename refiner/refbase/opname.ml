(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
 *
 * $Log$
 * Revision 1.3  1998/06/12 18:36:29  jyh
 * Working factorial proof.
 *
 * Revision 1.2  1998/06/01 13:54:44  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 15:00:17  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.2  1998/04/24 02:42:44  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1997/04/28 15:51:27  jyh
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
 * Revision 1.2  1996/09/02 19:43:20  jyh
 * Semi working package management.
 *
 * Revision 1.1  1996/04/07 18:27:08  jyh
 * Intermediate checking while updating dform commands.
 *
 *)

open Printf
open Debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Opname%t" eflush

let debug_opname =
   create_debug (**)
      { debug_name = "opname";
        debug_description = "display opname construction";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type opname = string list

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * We hash-cons the opnames.
 *)
let (optable : (opname, opname) Hashtbl.t) = Hashtbl.create 97

(*
 * Constructors.
 *)
let nil_opname = []

let _ = Hashtbl.add optable nil_opname nil_opname

let mk_opname s name =
   let op = s::name in
      try Hashtbl.find optable op with
         Not_found -> Hashtbl.add optable op op; op

let make_opname =
   let rec aux = function
      [] -> nil_opname
    | h::t -> mk_opname h (aux t)
   in
      aux

let normalize_opname = make_opname

(*
 * Destructor.
 *)
let dest_opname op = op

(*
 * Flatten the opname into a single string.
 *)
let rec flat_opname = function
   [h] -> h
 | h::t -> h ^ "!" ^ (flat_opname t)
 | [] -> "<null-opname>"

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
