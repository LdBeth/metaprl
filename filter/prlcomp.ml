(*
 * Front end to ocaml.
 *
 * Call it as:
 *    ocamlc -pp "camlp4 pa_o.cmo pa_op.cmo pr_dump.cmo pa_extend.cmo q_ast.cmo\
 *                   $NLLIB/refiner/refiner.cma\
 *                   $NLLIB/filter/prlcomp.cma\
 *                   $NLLIB/filter/filter_main.cmo"
 *       $*
 *
 * Set NL_INCLUDE from collection of all -I options.
 * Use "-E" option for preprocessing:
 *     camlp4 pa_o.cmo pa_op.cmo pr_dump.cmo pa_extend.cmo q_ast.cmo\
 *                   $NLLIB/refiner/refiner.cma\
 *                   $NLLIB/filter/prlcomp.cma\
 *                   $NLLIB/filter/filter_main.cmo
 *                   $*
 *)

open Printf
open Arg

open Debug

open Filter_magic

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Prlcomp%t" eflush;
   Debug_set.init ()

(*
 * Flags.
 *)
let preprocess_flag = ref false
let binary_flag = ref false
let optimize_flag = ref false
let verbose_mode = ref 0

(*
 * Environment.
 *)
let lib =
   ref (try Sys.getenv "NLLIB" with
           Not_found ->
              "/usr/local/lib/nuprl-light")

let set_lib s =
   let var = sprintf "NLLIB=%s" s in
      if !verbose_mode > 0 then
         eprintf "Setting %s%t" var eflush;
      Punix.putenv var;
      lib := s

(*
 * See if the file is interactive by checking the magic number.
 *)
let file_interactive file =
   try
      let inx = open_in_bin file in
         try
            let magic = input_binary_int inx in
               close_in inx;
               List.mem magic interactive_magics
         with
            _ ->
               close_in inx;
               false
   with
      Sys_error _ ->
         false

(*
 * Collect argument list.
 *)
let argv = ref []

let add_arg arg =
   argv := !argv @ [arg]

let add_binary_arg code name =
   binary_flag := true;
   argv := !argv @ [code; name]

let add_binary_if_newer code arg o_file raw_file term_file =
   if file_interactive raw_file then
      begin
         eprintf "File %s is interactive, compiling raw file%t" raw_file eflush;
         add_binary_arg code raw_file
      end
   else if file_interactive term_file then
      begin
         eprintf "File %s is interactive, compiling term file%t" term_file eflush;
         add_binary_arg code raw_file
      end
   else
      add_arg arg

let add_anon_arg arg =
   if Filename.check_suffix arg ".cmiz" or Filename.check_suffix arg ".cmit" then
      add_binary_arg "-intf" arg
   else if Filename.check_suffix arg ".cmoz" or Filename.check_suffix arg ".cmot" then
      add_binary_arg "-impl" arg
   else if Filename.check_suffix arg ".ml" then
      let root = Filename.chop_suffix arg ".ml" in
         add_binary_if_newer "-impl" arg (root ^ ".cmo") (root ^ ".cmoz") (root ^ ".cmot")
   else if Filename.check_suffix arg ".mli" then
      let root = Filename.chop_suffix arg ".mli" in
         add_binary_if_newer "-intf" arg (root ^ ".cmo") (root ^ ".cmiz") (root ^ ".cmit")
   else
      begin
         eprintf "Don't know how to compile %s, aborting%t" arg eflush;
         raise (Failure "add_anon_arg")
      end

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
 * Executable names.
 *)
let exe_name =
   let ostype =
      try Sys.getenv "OSTYPE" with
         Not_found ->
            ""
   in
   let win32 = Sys.os_type = "Win32" or ostype = "win32" or ostype = "cygwin32" in
      if win32 then
         (fun name -> name ^ ".exe")
      else
         (fun name -> name)

let ocamlc_exe   = exe_name "ocamlc"
let ocamlopt_exe = exe_name "ocamlopt"
let camlp4o_exe  = exe_name "camlp4o"
let camlp4n_exe  = exe_name "camlp4n"
let prlcn_exe    = exe_name "prlcn"
let prlco_exe    = exe_name "prlco"

(*
 * Commands are constructed from argument list.
 *)
let mk_camlp4 argv includes =
   (Filename.concat !lib camlp4o_exe) :: argv

let mk_ocamlc argv includes =
   let ocamlc =
      if !optimize_flag then
         ocamlopt_exe
      else
         ocamlc_exe
   in
      [ocamlc; "-I"; !lib; "-pp"; Filename.concat !lib camlp4n_exe] @ argv

let mk_prlcn argv includes =
   let rec mk_includes = function
      h::t ->
         " -I " ^ h ^ (mk_includes t)
    | [] ->
         ""
   in
   let preproc = (Filename.concat !lib prlcn_exe) ^ (mk_includes includes) in
      [ocamlc_exe; "-I"; !lib; "-pp"; preproc] @ argv

let mk_prlco argv includes =
   Filename.concat !lib prlco_exe :: argv

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
    "-opt", Set optimize_flag, "use the optimizing compiler";
    "-v", Unit (fun () -> verbose_mode := 1), "set verbose mode";
    "-V", Unit (fun () -> verbose_mode := 2), "set verbose mode";
    "-g", Unit (add_argv "-g"), "include debugging information";
    "-p", Unit (add_argv "-p"), "include profiling information";
    "-c", Unit (add_argv "-c"), "produce object file";
    "-o", String (add_string_argv "-o"), "specify output file";
    "-a", Unit (add_argv "-a"), "produce archive file";
    "-S", Unit (add_argv "-S"), "do not remove the assembly file";
    "-compact", Unit (add_argv "-compact"), "produce a smaller but slower code";
    "-custom", Unit (add_argv "-custom"), "generate custom executable";
    "-ccopt", String (add_string_argv "-ccopt"), "C option";
    "-cclib", String (add_string_argv "-cclib"), "C library";
    "-inline", String (add_string_argv "-inline"), "Inline level";
    "-linkall", Unit (add_argv "-linkall"), "specify link";
    "-lib", String set_lib, "set the library directory";
    "-noassert",Unit (add_argv "-noassert"), "Don't compile assertion checks"]

(*
 * Print the command line.
 *)
let print_command_line argv =
   let printer s =
      if !verbose_mode = 2 then
         eprintf "'%s'%t" s eflush
      else
         eprintf "%s " s
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
   let argv' = Array.map String.copy argv' in
   let _ =
      if !verbose_mode <> 0 then
         print_command_line argv
   in
(*
   let env = Unix.environment () in
   let pid = Unix.create_process_env argv'.(0) argv' env Unix.stdin Unix.stdout Unix.stderr in
      match snd (Unix.waitpid [] pid) with
         Unix.WEXITED code ->
            eprintf "Compiler exited with code %d%t" code eflush;
            exit code
       | Unix.WSIGNALED _
       | Unix.WSTOPPED _ ->
            exit 1
*)
      Punix.execvp argv'.(0) argv';
      eprintf "Execution failed: %s%t" argv'.(0) eflush;
      exit 1

let _ = Printexc.catch (Unix.handle_unix_error main) ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
