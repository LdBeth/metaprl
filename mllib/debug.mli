(*
 * Debugging tools.
 *)

(*
 * Particular debug variables.
 *)
val debug_load : bool ref

(*
 * Info about debug variables.
 * The variables themselves are defined in the Debug module.
 *)
type debug_info =
   { debug_name : string;
     debug_description : string;
     debug_value : bool
   }

(*
 * We create named debug variables.
 *)
val create_debug : debug_info -> bool ref
val load_debug : string -> bool ref

(*
 * Operations to inspect debug flags.
 *)
val set_debug : string -> bool -> unit
val get_debug : string -> debug_info
val debuggers : unit -> debug_info array

(*
 * We allow flags to be set from the environment.
 * they may be set before the vars are created,
 * so we add them as "possible" debug flags,
 * then check them later.
 *)
val set_possible_debug : string -> bool -> unit
val check_debug : unit -> unit

(*
 * Print a list of strings.
 *)
val print_any_list : (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit
val print_string_list :  out_channel -> string list -> unit
val print_int_list :  out_channel -> int list -> unit

(*
 * Flush output.
 *)
val eflush : out_channel -> unit

(*
 * $Log$
 * Revision 1.7  1998/06/22 19:45:27  jyh
 * Rewriting in contexts.  This required a change in addressing,
 * and the body of the context is the _last_ subterm, not the first.
 *
 * Revision 1.6  1998/06/12 13:46:47  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.5  1998/05/01 14:59:28  jyh
 * Updating display forms.
 *
 * Revision 1.4  1998/04/28 18:30:25  jyh
 * ls() works, adding display.
 *
 * Revision 1.3  1998/04/24 02:42:27  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/02/23 14:46:32  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1997/08/06 16:17:52  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:16  jyh
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
 * Revision 1.4  1996/03/08 15:40:37  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.3  1996/02/18 23:32:25  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.2  1996/02/14 03:51:48  jyh
 * This is a version common to Caml-Light and Caml-Special-Light.
 *
 * Revision 1.1  1996/02/13 21:32:05  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
