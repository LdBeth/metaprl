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
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Prlcomp%t" eflush;
   Debug_set.init ()

(*
 * Environment.
 *)
let lib =
   try Sys.getenv "NLLIB" with
      Not_found ->
         "/usr/local/lib/nuprl-light"

(*
 * Flags.
 *)
let preprocess_flag = ref false
let binary_flag = ref false
let verbose_mode = ref 0

(*
 * Collect argument list.
 *)
let argv = ref []
    
let add_arg arg =
   argv := !argv @ [arg]

let add_anon_arg arg =
   if Filename.check_suffix arg ".cmiz" or
      Filename.check_suffix arg ".cmit"
   then
      begin
         binary_flag := true;
         argv := !argv @ ["-intf"; arg]
      end
   else if Filename.check_suffix arg ".cmoz" or
           Filename.check_suffix arg ".cmot"
   then
      begin
         binary_flag := true;
         argv := !argv @ ["-impl"; arg]
      end
   else
      add_arg arg

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
   let var = sprintf "NL_INCLUDE=%s" (squash !includes) in
      if !verbose_mode > 0 then
         eprintf "%s%t" var eflush;
      Punix.putenv var;
      ()

(*
 * Commands are constructed from argument list.
 *)
let mk_camlp4 argv includes =
   (Filename.concat lib "camlp4o.run") :: argv

let mk_ocamlc argv includes =
   ["ocamlc"; "-I"; lib; "-pp"; Filename.concat lib "camlp4n.run"] @ argv

let mk_prlcn argv includes =
   let rec mk_includes = function
      h::t ->
         " -I " ^ h ^ (mk_includes t)
    | [] ->
         ""
   in
   let preproc = (Filename.concat lib "prlcn.run") ^ (mk_includes includes) in
      ["ocamlc"; "-I"; lib; "-pp"; preproc] @ argv

let mk_prlco argv includes =
   Filename.concat lib "prlco.run" :: argv

let mk_command () =
   let f =
      if !preprocess_flag then
         if !binary_flag then
            mk_prlco
         else
            mk_camlp4
      else if !binary_flag then
         mk_prlcn
      else
         mk_ocamlc
   in
      f !argv !includes

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
   let spec = spec @ Env_arg.args () in
   let _ = Env_arg.parse spec add_anon_arg "Nuprl-Light compiler" in
   let _ = set_includes () in
   let argv = mk_command () in
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
 * Revision 1.11  1998/04/28 18:30:20  jyh
 * ls() works, adding display.
 *
 * Revision 1.10  1998/04/24 19:38:41  jyh
 * Updated debugging.
 *
 * Revision 1.9  1998/04/24 02:42:20  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.8  1998/02/23 14:46:26  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.7  1998/02/18 18:46:32  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.6  1998/02/12 23:38:21  jyh
 * Added support for saving intermediate files to the library.
 *
 * Revision 1.5  1998/01/27 23:04:20  jyh
 * Adding OCaml1.07 syntax.
 *
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
