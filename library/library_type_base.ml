(*
 * This implements a filesystem interface to the library.
 *)

open File_base_type

open Term

(*
 * Save a term to the library.
 * "Magic" is a magic number that is sued to identify the
 * version of the file.
 *)
let library_set magic filename =
   raise (Failure "storing a file to the library is not implemented")

(*
 * Get a term from the library.
 *)
let library_get magic filename =
   raise (Failure "retriving a file from the library is not implemented")

(*
 * This "combo" is the module that defines how to fetch
 * an object from the library.  We are passed an argument
 * that describes how to marshal and unmarshal objects to terms.
 *)
module IO =
struct
   type t = term
   let write = library_set
   let read  = library_get
end

(*
 * $Log$
 * Revision 1.3  1998/02/19 17:24:13  jyh
 * Splitting filter_parse.
 *
 * Revision 1.2  1998/02/18 18:46:37  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.1  1998/02/12 23:35:44  jyh
 * Added base Nuprl-Light interface to the library.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
