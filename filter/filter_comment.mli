(*
 * This module is used to provide an association between
 * terms in an ML file, and their comments.
 *
 * The algorithm is best match.  We parse the comments from
 * the file, the iterate through all the terms in the
 * program.  The closest, largest program block after the comment
 * is associated with the comment through a table.
 * The comments can then be queried through the table.
 *)

(*
 * Type of comment associations.
 *)
type t

(*
 * Read the comments from a file.
 * The argument is the name of the file,
 * and the result is the list of comments with
 * their offset into the file.
 *
 * Raises Sys_error if the file does not exist.
 *)
val parse : string -> (int * string) list

(*
 * Create an association.
 *)
val create_sig : (int * string) list -> MLast.sig_item list -> t
val create_str : (int * string) list -> MLast.str_item list -> t

(*
 * Query the association.
 * Raises Not_found
 *)
val get : t -> MLast.loc -> int * string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
