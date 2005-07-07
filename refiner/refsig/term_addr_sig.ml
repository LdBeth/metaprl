(*
 * Addressed operations on terms.
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
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_symbol

(*
 * Sequent addresses - conclusion is clause 0; hyps are clauses 1 through n
 * (numbering starts at the left), or -n through -1, with -1 referring to the
 * last one. ArgAddr is the sequent_arg.
 * Term addresses - Subterm i is i-th immediate subterm (numbering starts at 1).
 * Negative numbers are allowed in Subterm as well (counting starts from the
 * right). "Subterm 0" is illegal.
 *)
type addr_item =
   Subterm of int
 | ArgAddr
 | ClauseAddr of int

let compose_addr = (( @ ) : (addr_item list -> addr_item list -> addr_item list))

(*
 * Format address as a string.
 *)
let string_of_addr =
   let rec aux = function
      [] -> ""
    | [Subterm i] -> string_of_int i
    | [ArgAddr] -> "Arg"
    | [ClauseAddr i] -> "Clause(" ^ string_of_int (i+1) ^ ")"
    | addr1 :: ( (_ :: _) as addr2) -> (aux [addr1]) ^ "; " ^ (aux addr2)
in
   (fun addr -> "[" ^ aux addr ^ "]")

let nth_clause_addr i = [ ClauseAddr i ]
let concl_addr = [ ClauseAddr 0 ]
let nth_hyp_addr = nth_clause_addr

(*
 * Traslate a [-lenght..-1]U[1..length] index into a [0..length-1] one.
 *)
let make_index_opt i length =
   if i = 0 then
      raise (Invalid_argument "Term_addr_ds.make_index_opt: got Subterm 0")
   else if i > 0 then
      if i > length then
         None
      else
         Some (i - 1)
   else
      let i = length + i in
         if i < 0 then
            None
         else
            Some i

module type TermAddrSig =
sig
   module AddrTypes : Term_sig.TermSig
   open AddrTypes

   type address

   (*
    * Constructors/destructors
    *)
   val make_address : addr_item list -> address
   val dest_address : address -> addr_item list
   val compose_address : address -> address -> address

   (* Works only on sequent addresses and their subaddresses. *)
   (* XXX TODO: Should go away once a proper crw mechanism is implemented. *)
   val split_clause_address : address -> address * address

   (*
    * Destructors.
    *)
   val string_of_address : address -> string

   (*
    * Addressed operations.
    * All functions (including apply*fun_higher), except for subterm_exists,
    * when given an address that does not exist in a term, may chose to pick
    * a "random" existing address or to raise a RefineError.
    * In all the functions below SymbolSet.t stands for binding vars.
    *)
   val subterm_exists : term -> address -> bool
   val find_subterm : term -> (term -> SymbolSet.t -> bool) -> address list
   val term_subterm :  term -> address -> term
   val replace_subterm : term -> address -> term -> term
   val apply_fun_at_addr : (term -> term) -> address -> term -> term
   val apply_fun_arg_at_addr : (term -> term * 'a) -> address -> term -> term * 'a
   val apply_var_fun_at_addr : (SymbolSet.t -> term -> term) -> address -> SymbolSet.t -> term -> term
   val apply_var_fun_arg_at_addr : (SymbolSet.t -> term -> term * 'a) -> address -> SymbolSet.t -> term -> term * 'a

   (*
    * Adresses of immediate subterms
    *)
   val subterm_addresses : term -> address list

   (*
    * Strip the initial part of an address.
    *)
   val strip_address : addr_item list -> address -> address

   (*
    * higherC low-level implementation
    *)
   val apply_fun_higher : (term -> term * 'a) -> term -> term * 'a list
   val apply_var_fun_higher : (SymbolSet.t -> term -> term * 'a) ->
      SymbolSet.t -> term -> term * 'a list

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
