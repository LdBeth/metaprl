(*
 * Shared types for printing ML terms.
 *)

open Term
open Term_util

(*
 * Generic file.
 *)
module type FileSig =
sig
   (* File type *)
   type t
   
   (* Input type *)
   type name
   
   (* Output type *)
   type out
   
   (* Creation *)
   val create : name -> t
   val close : t -> unit
   
   (* Output operations *)
   val puti : t -> string -> unit
   val put : t -> string -> unit
   val get : t -> out
end

(*
 * Generic printer.
 *)
module type PrinterSig =
sig
   (* File type *)
   type t
   
   (* Printers *)
   val print_term : t -> term -> unit
   val print_named_term : t -> string -> term -> unit
   val print_mterm : t -> meta_term -> unit
   val print_term_list : t -> term list -> unit
end

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:24  jyh
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
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
