(*
 * Debugging tools.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

(*
 * See doc/htmlman/developer-guide/debugging.html or
 * http://cvs.metaprl.org:12000/metaprl/developer-guide/debugging.html
 * for more information on how to use this module
 * and on MetaPRL debugging in general.
 *)

(* if "load" debug is true, `show_load (s ^ "%t")' will print s to stderr and flush stderr *)
val show_loading : ((out_channel -> unit) -> unit, out_channel, unit) format -> unit

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
val debug_usage : unit -> unit

(*
 * We allow flags to be set from the environment.
 * they may be set before the vars are created,
 * so we add them as "possible" debug flags,
 * then check them later.
 *)
val set_possible_debug : string -> bool -> unit

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
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
