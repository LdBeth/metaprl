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
 * Preprocess?
 *)
let preprocess_flag = ref false

let verbose_mode = ref 0

(*
 * Files.
 *)
let camlp4 () =
   ["camlp4";
    "pa_o.cmo";
    "pa_op.cmo";
    "pr_o.cmo";
    "pa_extend.cmo";
    "q_MLast.cmo";
    sprintf "%s/mllib/util.cma" !root;
    sprintf "%s/refiner/refiner.cma" !root;
    sprintf "%s/filter/prlcomp.cma" !root;
    sprintf "%s/filter/filter_main.cmo" !root]

let ocamlc () =
   ["ocamlc";
    "-pp";
    sprintf "camlp4 pa_o.cmo pa_op.cmo pr_dump.cmo pa_extend.cmo q_MLast.cmo \
%s/mllib/util.cma \
%s/refiner/refiner.cma \
%s/filter/prlcomp.cma \
%s/filter/filter_main.cmo"
    !root !root !root !root]

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

let set_includes () =
   let rec squash = function
      [h] -> h
    | h::t ->
         h ^ ":" ^ (squash t)
    | [] ->
         ""
   in
   let var = sprintf "REF_INCLUDE=%s" (squash !includes) in
      if !verbose_mode > 0 then
         eprintf "%s%t" var eflush;
      Punix.putenv var;
      ()

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
    "-a", Unit (add_argv "-a"), "produce archive file";
    "-custom", Unit (add_argv "-custom"), "generate custom executable";
    "-cclib", String (add_string_argv "-cclib"), "C library";
    "-linkall", Unit (add_argv "-linkall"), "specify link"]

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
   let _ = set_includes () in
   let argv =
      if !preprocess_flag then
         camlp4 () @ !argv
      else
         ocamlc () @ !argv
   in
   let argv' = Array.of_list argv in
   let _ =
      if !verbose_mode <> 0 then
         print_command_line argv
   in
(*
   let pid = Unix.create_process_env argv'.(0) argv' env' Unix.stdin Unix.stdout Unix.stderr in
      match snd (Unix.waitpid [] pid) with
         Unix.WEXITED code ->
            eprintf "Compiler exited with code %d%t" code eflush;
            exit code
       | Unix.WSIGNALED _
       | Unix.WSTOPPED _ ->
 exit 1
 *)
      Unix.execvp argv'.(0) argv';
      eprintf "Execution failed: %s%t" argv'.(0) eflush;
      exit 1

let _ = Printexc.catch (Unix.handle_unix_error main) ()

(*
 * $Log$
 * Revision 1.4  1997/09/08 15:02:19  jyh
 * This version compiles Ensemble.
 *
 * Revision 1.3  1997/08/06 16:17:38  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.2  1997/05/05 21:04:48  jyh
 * Changing filter_p4 to filter.
 *
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
