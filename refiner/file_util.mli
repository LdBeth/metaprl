(*
 * Operations on files.
 *)

(* Can't open and can't find a file *)
exception CantOpen of string
exception CantFind of string

(*
 * Open a file somewhere in the path if possible.
 *)
val open_in_path : string list -> string -> in_channel * string

(*
 * Safe file handling.
 * the files are closed on exception.
 *)
val with_input_file : string -> (in_channel -> 'a) -> 'a
val with_output_file : string -> (out_channel -> 'a) -> 'a

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:21  jyh
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
