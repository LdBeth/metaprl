(*
 * Make a combo to read from the library.
 *)

open Term

open File_base_type

val maybe_lib_open : unit -> unit

module IO : IOSig with type t = term

(*
 * $Log$
 * Revision 1.3  1998/04/03 18:07:34  eaton
 * .
 *
# Revision 1.2  1998/02/19  17:24:13  jyh
# Splitting filter_parse.
#
 * Revision 1.1  1998/02/12 23:35:45  jyh
 * Added base Nuprl-Light interface to the library.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
