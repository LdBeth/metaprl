(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
 *)

open Printf
open Debug
open Gc

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

open Gc;;

(* This changes the GC parameters for the whole Nuprl-Light system *)

let r = Gc.get () in
(*   r.verbose <- true;  *)
   r.minor_heap_size <- 196608;
   r.major_heap_increment <- 393216;
   r.space_overhead <- 70;
   Gc.set r;;

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
      [] ->
         nil_opname
    | h::t ->
         mk_opname h (aux t)
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
 * Print as a string.
 * The opname has to be reversed.
 * This function is overly long,
 * but it is efficient.
 *)
let string_of_opname = function
   [] ->
      ""
 | h::t ->
      let rec collect s = function
         h::t ->
            collect (h ^ "!" ^ s) t
       | [] ->
            s
      in
         collect h t

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
