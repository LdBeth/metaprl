(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:42  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.2  1996/09/02 19:43:20  jyh
 * Semi working package management.
 *
 * Revision 1.1  1996/04/07 18:27:08  jyh
 * Intermediate checking while updating dform commands.
 *
 *)

open List;;
open Hashtbl;;

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type opname = string list;;

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * We hash-cons the opnames.
 *)
let (optable : (opname, opname) Hashtbl.t) = Hashtbl.create 97;;

(*
 * Constructors.
 *)
let nil_opname = [];;
add optable nil_opname nil_opname;;

let mk_opname s name =
   let op = s::name in
      try find optable op with
         Not_found -> add optable op op; op;;

let make_opname =
   let rec aux = function
      [] -> nil_opname
    | h::t -> mk_opname h (aux t)
   in
      aux;;

let normalize_opname = make_opname;;

(*
 * Destructor.
 *)
let dest_opname op = op;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
