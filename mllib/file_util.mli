(*
 * Operations on files.
 *)

(* Can't open and can't find a file *)
exception CantOpen of string
exception CantFind of string

(*
 * Utilities on filenames.
 *)
val parse_path : string list -> string -> string list
val build_path : string list -> string
val path_dir : string -> string
val path_file : string -> string

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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
