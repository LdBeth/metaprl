(*
 * Front end to ocaml.
 *
 * Call it as:
 *    ocamlc -pp "camlp4 pa_o.cmo pa_op.cmo pr_dump.cmo pa_extend.cmo q_ast.cmo\
 *                   $REF_ROOT/refiner/refiner.cma\
 *                   $REF_ROOT/filter/prlcomp.cma\
 *                   $REF_ROOT/filter/filter_main.cmo"
 *       $*
 *
 * Set REF_INCLUDE from collection of all -I options.
 * Get REF_ROOT from the environment ("." as default).
 * Use "-E" option for preprocessing:
 *     camlp4 pa_o.cmo pa_op.cmo pr_dump.cmo pa_extend.cmo q_ast.cmo\
 *                   $REF_ROOT/refiner/refiner.cma\
 *                   $REF_ROOT/filter/prlcomp.cma\
 *                   $REF_ROOT/filter/filter_main.cmo
 *                   $*
 *)

open Printf
open Arg

open Debug

(*
 * Environment.
 *)
let string_set _ s v =
   s := v

let root = Env_arg.string "root" ".." "root directory of Nuprl-light" string_set

(*
 * Files.
 *)
let camlp4 () =
   ["camlp4";
    "pa_o.cmo";
    "pa_op.cmo";
    "pr_o.cmo";
    "pa_extend.cmo";
    "q_ast.cmo";
    sprintf "%s/refiner/refiner.cma" !root;
    sprintf "%s/filter_p4/prlcomp.cma" !root;
    sprintf "%s/filter_p4/filter_main.cmo" !root]

let ocamlc () =
   ["ocamlc";
    "-pp";
    sprintf "camlp4 pa_o.cmo pa_op.cmo pr_dump.cmo pa_extend.cmo q_ast.cmo %s/refiner/refiner.cma %s/filter_p4/prlcomp.cma %s/filter_p4/filter_main.cmo" !root !root !root]

(*
 * Collect argument list.
 *)
let argv = ref []
    
let add_arg arg =
   argv := !argv @ [arg]

let add_argv arg () =
   add_arg arg

let add_string_argv arg s =
   add_arg arg;
   add_arg s

(*
 * Collect include directory specification.
 *)
let includes = ref []
    
let add_include dir =
   includes := !includes @ [dir];
   argv := !argv @ ["-I"; dir]

let get_includes () =
   let rec squash = function
      [h] -> h
    | h::t ->
         h ^ ":" ^ (squash t)
    | [] ->
         ""
   in
      sprintf "REF_INCLUDE=%s" (squash !includes)

(*
 * Preprocess?
 *)
let preprocess_flag = ref false

let verbose_mode = ref 0

(*
 * Arguments.
 *)
let spec =
   ["-I", String add_include, "add an directory to the path for include files";
    "-E", Set preprocess_flag, "preprocess only";
    "-v", Unit (fun () -> verbose_mode := 1), "set verbose mode";
    "-V", Unit (fun () -> verbose_mode := 2), "set verbose mode";
    "-g", Unit (add_argv "-g"), "include debugging information";
    "-c", Unit (add_argv "-c"), "produce object file";
    "-o", String (add_string_argv "-o"), "specify output file";
    "-a", Unit (add_argv "-a"), "produce archive file"]

(*
 * Print the command line.
 *)
let print_command_line argv =
   let printer =
      if !verbose_mode = 2 then
         eprintf "'%s' "
      else
         eprintf "%s "
   in
   let rec print = function
      h::t ->
         printer h;
         print t
    | [] ->
         eflush stderr
   in
      output_string stderr "+ ";
      print argv

(*
 * Main function parses arguments, then issues command.
 *)
let main () =
   let _ = Env_arg.parse spec add_arg "Nuprl-Light compiler" in
   let env = Array.to_list (Unix.environment ()) in
   let env' = Array.of_list (get_includes () :: env) in
   let argv =
      if !preprocess_flag then
         camlp4 () @ !argv
      else
         ocamlc () @ !argv
   in
   let argv' = Array.of_list argv in
      if !verbose_mode <> 0 then
         print_command_line argv;
      Unix.execvpe argv'.(0) argv' env'

let _ = Printexc.catch (Unix.handle_unix_error main) ()

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:05  jyh
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
