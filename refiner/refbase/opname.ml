(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
 *)

open Printf
open Nl_debug
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

(*
 * This changes the GC parameters for the whole Nuprl-Light system.
 *)
let _ =
   let r = Gc.get () in
(*    r.verbose <- true;  *)
      r.Gc.minor_heap_size <- 196608;
      r.Gc.major_heap_increment <- 393216;
      r.Gc.space_overhead <- 70;
      Gc.set r

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Add a reference that the marshaler will never find,
 * so that we can do opname normalization lazily.
 *)
type token = string
type atom = string list

let opname_token = String.make 4 (Char.chr 0)

type opname =
   { mutable opname_token : token;
     mutable opname_name : string list
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * We hash-cons the opnames.
 *)
let (optable : (string list, opname) Hashtbl.t) = Hashtbl.create 97

(*
 * Constructors.
 *)
let nil_opname = { opname_token = opname_token; opname_name = [] }

let _ = Hashtbl.add optable [] nil_opname

let rec mk_opname s { opname_token = token; opname_name = name } =
   if token == opname_token then
      let name = s :: name in
         try Hashtbl.find optable name with
            Not_found ->
               let op = { opname_token = opname_token; opname_name = name } in
                  Hashtbl.add optable name op;
                  op
   else
      make_opname name

and make_opname = function
   [] ->
      nil_opname
 | h :: t ->
      mk_opname h (make_opname t)

let normalize_opname { opname_token = token; opname_name = name } =
   if token = opname_token then
      (*
       * This is for reverse compatibility with opnames made
       * from lists.
       *)
      make_opname name
   else
      make_opname (token :: name)

(*
 * Atoms are the inner string list.
 *)
let intern opname =
   if opname.opname_token == opname_token then
      opname.opname_name
   else
      let name = (normalize_opname opname).opname_name in
         opname.opname_token <- opname_token;
         opname.opname_name <- name;
         name

(*
 * Equality on opnames.
 * We try to make this efficient.
 * For performance testing compare the following:
 * let eq op1 op2 =
 *    1. op1 == op2
 *    2. op1.opname_name == op2.opname_name
 *    3. op1.opname_name = op2.opname_name
 *    4. the code below
 *
 * Options 1 and 2 will be semantically incorrect for
 * marshalled terms,  Option 3 may not be so bad really,
 * because opnames are usually small.
 *)
let eq_inner op1 op2 =
   op1.opname_token <- opname_token;
   op1.opname_name <- (normalize_opname op1).opname_name;
   op2.opname_token <- opname_token;
   op2.opname_name <- (normalize_opname op2).opname_name;
   op1.opname_name == op2.opname_name

let eq op1 op2 =
   (op1.opname_name == op2.opname_name)
   or ((op1.opname_token != opname_token or op2.opname_token != opname_token) & eq_inner op1 op2)

(*
 * Destructor.
 *)
let dest_opname { opname_name = name } =
   name

(*
 * Flatten the opname into a single string.
 *)
let flat_opname op =
   let rec flatten = function
      [h] ->
         h
    | h::t ->
         h ^ "!" ^ (flatten t)
    | [] ->
         "<null-opname>"
   in
      flatten op.opname_name

(*
 * Print as a string.
 * The opname has to be reversed.
 * This function is overly long,
 * but it is efficient.
 *)
let string_of_opname op =
   let rec flatten = function
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
   in
      flatten op.opname_name

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
