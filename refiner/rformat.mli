(*
 * Formatter like in the standard library.
 * Output is organized into boxes, each of which has an indentation.
 *
 * Commands:
 *    format_sbreak str str': soft break is taken if necessary
 *        if taken, str is printed after the current line
 *        if not, str' is printed
 *    format_break str str': hard break is takenin groups
 *        if taken, str is printed
 *        if not, str' is printed
 *
 *    format_lzone: begin a zone with no breaks
 *    format_szone: soft break zone (all or no hard breaks are taken)
 *    format_hzone: all hard breaks are taken.
 *    format_ezone: end the current zone.
 *
 *    format_pushm i: push left margin from here by i more spaces
 *    format_popm: pop last pushm
 *
 *    format_char: add a single char
 *    format_int: print a number
 *    format_string: add a string to the buffer
 *
 *)

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type buffer

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(*
 * Buffer creation.
 *)
val new_buffer : unit -> buffer
val clear_buffer : buffer -> unit

(*
 * Breaks.
 *)
val format_sbreak : buffer -> string -> string -> unit
val format_break : buffer -> string -> string -> unit
val format_ibreak : buffer -> string -> string -> unit
val format_space : buffer -> unit
val format_hspace : buffer -> unit
val format_newline : buffer -> unit

(*
 * Break zones.
 *)
val format_lzone : buffer -> unit
val format_szone : buffer -> unit
val format_hzone : buffer -> unit
val format_ezone : buffer -> unit

(*
 * MArgins.
 *)
val format_pushm : buffer -> int -> unit
val format_popm : buffer -> unit

(*
 * Printers.
 *)
val format_char : buffer -> char -> unit
val format_string : buffer -> string -> unit
val format_quoted_string : buffer -> string -> unit
val format_int : buffer -> int -> unit
val format_num : buffer -> Num.num -> unit
val format_buffer : buffer -> buffer -> unit

(*
 * Collecting output.
 *)
val print_to_channel : int -> buffer -> out_channel -> unit
val print_to_string : int -> buffer -> string

(*
 * $Log$
 * Revision 1.3  1998/03/20 22:16:20  eli
 * Eli: Changed integer parameters to Num.num's.
 *
 * Revision 1.2  1997/08/06 16:18:13  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:38  jyh
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
 * Revision 1.5  1996/05/21 02:14:14  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.4  1996/03/25 20:50:39  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.3  1996/02/25 15:16:13  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.2  1996/02/19 18:46:45  jyh
 * Updating format.prl
 *
 * Revision 1.1  1996/02/18 23:32:27  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
