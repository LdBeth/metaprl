(*
 * Build the toploop version.
 *)

open Printf
open Nl_debug

module Shell = Shell.Shell (Shell_nl.ShellP4)

let _ = Shell.main ()

external exit : int -> unit = "caml_exit"

let _ =
   eprintf "Nuprl-Light exiting%t" eflush;
   flush stdout;
   exit 0

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
