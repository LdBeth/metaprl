(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
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
