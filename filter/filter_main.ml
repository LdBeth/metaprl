(*
 * Main function collects arguments, and starts parsing.
 *)

open Arg
open Printf
open Nl_debug

open Filter_parse

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_main%t" eflush

(*
 * string -> path commands
 *)
let set_path doc var path =
   let path' = String_util.split ':' path in
      var := path'

let set_path_arg doc var =
   Arg.String (set_path doc var)

(*
 * This is a list of hosts to use for database lookup.
 *)
let include_path = Env_arg.general "include" ["."] "Include directories" set_path set_path_arg

let add_include path =
   include_path := !include_path @ [path]

let set_output_file name =
   Pcaml.output_file := Some name

let remove_output_file () =
   match !Pcaml.output_file with
      Some name ->
         Sys.remove name
    | None ->
         ()

let set_lib s =
   let var = sprintf "NLLIB=%s" s in
      Punix.putenv var;
      ()

let add_anon_arg arg =
   ()

let spec =
   ["-I", String add_include, "add an directory to the path for include files";
    "-o", String set_output_file, "specify output file";
    "-lib", String set_lib, "specify NLLIB directory"]

(*
 * Add the include path.
 *)
let main () =
   Arg.current := 1;
   Env_arg.parse spec add_anon_arg "Nuprl-Light compiler";
   set_include_path !include_path;
      try !Odyl_main.go () with
         exn ->
            remove_output_file ();
            raise exn

let _ =
   Printexc.catch main ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
