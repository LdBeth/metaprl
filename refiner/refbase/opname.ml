(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

open Lm_debug

(*
 * Kinds of operators.
 *)
type op_kind =
   NormalKind
 | TokenKind

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Opname%t"

let debug_opname =
   create_debug (**)
      { debug_name = "opname";
        debug_description = "display opname construction";
        debug_value = false
      }

(*
 * This changes the GC parameters for the whole MetaPRL system.
 *)
let _ =
   let r = Gc.get () in
      (* r.verbose <- 1; *)
      r.Gc.minor_heap_size <- 196608;
      r.Gc.major_heap_increment <- 1048576;
      r.Gc.space_overhead <- 85;
      r.Gc.stack_limit <- 524288;
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

let rec mk_opname s ({ opname_token = token; opname_name = name } as opname) =
   if token == opname_token then
      let name = s :: name in
         try Hashtbl.find optable name with
            Not_found ->
               let op = { opname_token = opname_token; opname_name = name } in
                  Hashtbl.add optable name op;
                  op
   else
      mk_opname s (normalize_opname opname)

and make_opname = function
   [] ->
      nil_opname
 | h :: t ->
      mk_opname h (make_opname t)

and normalize_opname opname =
   if opname.opname_token == opname_token then
      (* This opname is already normalized *)
      opname
   else
      let res = make_opname opname.opname_name in
         opname.opname_name <- res.opname_name;
         opname.opname_token <- opname_token;
         res

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
   op1.opname_name  <- (normalize_opname op1).opname_name;
   op1.opname_token <- opname_token;
   op2.opname_name  <- (normalize_opname op2).opname_name;
   op2.opname_token <- opname_token;
   op1.opname_name == op2.opname_name

let eq op1 op2 =
   (op1.opname_name == op2.opname_name)
   || ((op1.opname_token != opname_token || op2.opname_token != opname_token) && eq_inner op1 op2)

(*
 * Destructor.
 *)
let dst_opname = function
   { opname_name = n :: name; _ } ->
      n, { opname_token = opname_token; opname_name = name }
 | _ ->
      raise (Invalid_argument "dst_opname")

let dest_opname { opname_name = name; _ } =
   name

(*
 * Print as a string.
 * The opname has to be reversed.
 * This function is overly long,
 * but it is efficient.
 *)
let string_of_opname opname =
   match opname.opname_name with
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
 * Get the root of the opname.
 *)
let opname_root =
   let rec search names =
      match names with
         [] ->
            ""
       | [name] ->
            name
       | _ :: names ->
            search names
   in
      (fun opname -> search opname.opname_name)

(*
 * A few "special" opnames.
 *)
let var_opname     = make_opname ["var"]
let context_opname = make_opname ["context"]

(*
 * Manifest terms are injected into the "perv" module.
 *)
let xperv          = make_opname ["Perv"]
let sequent_opname = mk_opname "sequent" xperv
let xnil_opname    = mk_opname "xnil" xperv
let xcons_opname   = mk_opname "xcons" xperv
let xconcl_opname  = mk_opname "xconcl" xperv

(************************************************************************
 * Sets and tables.
 *)
let compare { opname_name = name1; _ } { opname_name = name2; _ } =
   Stdlib.compare name1 name2

module OpnameCompare =
struct
   type t = opname
   let compare = compare
end

module OpnameSet = Lm_set.LmMake (OpnameCompare)
module OpnameTable = Lm_map.LmMake (OpnameCompare)
module OpnameMTable = Lm_map.LmMakeList (OpnameCompare)

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
